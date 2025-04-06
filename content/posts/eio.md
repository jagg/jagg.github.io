+++
title = "Notes on Eio"
date = 2025-04-05T10:01:15+02:00
images = []
tags = ["asyn", "networking", "eio"]
categories = ["OCaml"]
draft = false
+++

This weekend I took a step back from working on my Key-Value store to
make sure I understand the concurrency model of Eio, the OCaml 5
library that I'm using for concurrency and parallelism.

I decided to turn my notes into a post to force myself to address any
gaps in my understanding. I'm sure that there are several errors, or
things to improve, so if you are reading this and find some, please,
ping me on [Twitter](https://x.com/jagarciagim)!

# What is Eio?
Eio is an Effects-Based Parallel IO library for OCaml 5. It uses the
new multicore features, allowing programs to use several cores. It
also supports several backends, defaulting to
[io_uring](https://unixism.net/loti/what_is_io_uring.html) in linux!
This means that we can use Eio to do async IO (it comes with a socket
and file API), as well as using parallel execution on different
threads (called domains).

# Concepts
There are a bunch of concepts you will see through the documentation,
I think it's useful to define them at a high level before we get into
details:

- Domain: Domains are the units of parallelism in OCaml. A Domain maps
to an OS thread, so you should not spawn more domains than the
number of cores available in the system.

- Fiber: A light-weight thread. For a single domain, only one Fiber
can be running at a time. A Fiber runs until it performs an IO
operation, or until it explicitely calls `Fiber.yield`.

- Switch: A switch is a grouping of Fibers, so that they can be waited
on together. `Switch.run` waits until the function and all the
attached Fibers finish running, and releases attached resources,
like file handles once they are done.

- Domain Manager: Provides the basic API to spawn new Domains that
will execute in parallel. It can be used to spawn new Domains, but
in general it's used through an Executor Pool.

- Executor Pool: An executor pool can be configured with a count of
Domains (usually no more than your CPU count). Then you can submit
work to the pool, that will be executed on separate Domains. This
allows reuse of previous Domains, so it's more efficient than
spawning new ones every time.

- Promise: They are a way to communicate between Fibers (even when
they live in different domains. One Fiber can wait on a Promise while
the other resolves it by putting the expected value inside.

- Streams: The are bounded, blocking queues. They can also be used to
communicate between Domains and Fibers. You can specify a maximum
capacity, if the queue is full, the sender will wait until there is
room. Also, if the queue is empty, the receiver will wait until
something shows up.

# How do we use all those things?

We can see it through an example. In my Key-Value store I have a
server that listens on a particular TCP port, I want it to receive
queries (Get and Set operations) on several threads.

In this version, I also want to serialize all the operations when they
go through the storage, so I would like to have only one domain
writing to the store.

I also want to make sure that all my writes are committed before I
return to the client.

How can we use the pieces above to get this?

## The TCP Listener

The
[Eio.Net](https://ocaml-multicore.github.io/eio/eio/Eio/Net/index.html)
module provides a ready to use listener. We only need to give it a
Switch to spawn fibers, and the `net` capability (this is part of the
Effects side of things, out of scope today, but there is a bit about
it in the [Eio
docs](https://github.com/ocaml-multicore/eio?tab=readme-ov-file#design-note-capabilities))

When we run the TCP server, we provide the socket on which we listen,
the amount of domains we want to use, and two handler functions, one
to use to answer connections, and another one to handle errors.

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Switch.run ~name:"Server" @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 12342) in
  let net = Eio.Stdenv.net env in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  let store = Store.make sw in
  traceln "[SERVER] Server ready!";
  Fiber.fork ~sw (fun () ->
      Eio.Net.run_server socket (handle_client store)
        ~additional_domains:(dm, 2)
        ~on_error:(traceln "Error found: %a" Fmt.exn))
```

## The Store

The store will need to spawn a domain to handle the gets/writes,
so that we don't have to worry about synchronization issues for
now. As we said above, we want three things:

1. We want to serialize all reads and writes for now, so we want only
one Domain writing at a time.
2. We need the Store Domain to get the work from the TCP server
workers, when available. This seems like a good use case for a
Stream!
3. But then, we don't want to return to the user until we have a
response from the Store. We can model that with a Promise.

The Store will process commands, which it will read from a Stream, and
return a response through a Promise.

```ocaml
type command =
  | Set of Model.Key.t * Model.value * (string Promise.u)
  | Get of Model.Key.t * (Model.value option Promise.u)

type t = command Eio.Stream.t

```

It will also need an Executor Pool to submit the worker function. And
of course, it will have to process the commands:

```ocaml
let make sw pool =
  let stream = Eio.Stream.create 120 in
  let table = Kvlib.Storage.create () in
  let rec handler () =
    match Eio.Stream.take stream with
    | Set (key, value, resolver) ->
      let _ = match Kvlib.Storage.put table ~key ~value with
        | Ok _ ->
          Promise.resolve resolver "Commited";
        | Error e ->
          traceln "Error found storing value: %s" (Error.to_string_hum e);
          Promise.resolve resolver @@ Error.to_string_hum e;
      in
      handler ()
    | Get (key, resolver) ->
      let value = Kvlib.Storage.get table ~key in
      Promise.resolve resolver value;
      handler ()
  in
  Fiber.fork ~sw (fun () ->
      Eio.Executor_pool.submit_exn pool
        ~weight:1.0
        handler);
  stream
```

Then, the server can just call `get` and `set` operations, and wait on the
promise for a response:

```ocaml
let set store key value =
  let promise, resolver  = Promise.create () in
  Eio.Stream.add store (Set (key, value, resolver));
  (** We don't want to return until we know it's been committed *)
  Promise.await promise

let get store key =
  let promise, resolver  = Promise.create () in
  Eio.Stream.add store (Get (key, resolver));
  Promise.await promise
```

Now we have to change our main function a bit to create the pool and
pass it along:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Switch.run ~name:"Server" @@ fun sw ->
  let dm = Eio.Stdenv.domain_mgr env in
  let pool = Eio.Executor_pool.create ~sw ~domain_count:2 dm in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 12342) in
  let net = Eio.Stdenv.net env in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  let store = Store.make sw pool in
  traceln "[SERVER] Server ready!";
  Fiber.fork ~sw (fun () ->
      Eio.Net.run_server socket (handle_client store)
        ~additional_domains:(dm, 2)
        ~on_error:(traceln "Error found: %a" Fmt.exn))
```

# Next steps and open questions

The approach above works, but there are several things that I haven't
investigated in detail yet. 

The backend for the store is currently using sync IO, which is fine
for now, but it would make sense to use the Filesystem capabilities in
Eio to handle this. The problem is that the
[API](https://ocaml-multicore.github.io/eio/eio/Eio/Path/index.html)
doesn't expose all the flags that I would like to use (O_DIRECT &
O_SYNC, see [this
post](https://transactional.blog/how-to-learn/disk-io) for details). I
think I should be able to wrap the Unix module and use it inside a
fiber, but I haven't done it yet. Also, the Unix module doesn't expose
O_DIRECT either...

On a different note, this is the first time I come across an Effect
system, I need to play with it more to make sure I understand how it
works!

All this code is available in [Github](https://github.com/jagg/ocledis)!
