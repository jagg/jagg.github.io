+++
title = "Giving OCaml a try"
date = 2025-03-22T20:31:47+01:00
images = []
tags = ["experiment", "networking", "async"]
categories = ["OCaml"]
draft = false 
+++

During the last couple of weeks I've gone on a completely different
direction. I happened to hear a few people saying good things about OCaml,
and I was kind of missing coding with strong types, so I decided to
reimplement my basic key value store (see the [post]({{% ref "cledis"%}})),
and see if it was fun to write.

First of all, here is the
[result](https://github.com/jagg/ocledis). The functionality is the
same, a hash map over a network, using an "official" async library,
and a basic client and server that receive and send S-Expressions over
the wire.

## What did I like?

### The libraries I found were pretty nice!
For async and networking I picked
[Eio](https://github.com/ocaml-multicore/eio), which is fairly modern,
and based on io_uring. For Common Lisp I used
[cl-async](https://github.com/orthecreedence/cl-async), which worked
very well, but it seems it hasn't had much activity since 2015.

Also, it turns out that OCaml has
[preprocessors](https://ocaml.org/docs/metaprogramming) able to
generate code. It seems that they have access to the Abstract Syntax
Tree of the sources, so they sound like something similar to Common
Lisp macros. I used it to autogenerate the code to transform my types
to S-Expressions, and it was really convenient!

### The build system and tooling just works
Installing the environment through Opam was painless, and setting a
project through Dune was very intuitive. Slightly easier than Common
Lisp's ASDF, and massively better than CMake!

### The type system was brilliant
I generally prefer strongly typed languages, and OCaml didn't
disappoint. The type inference allowed me iterate on my changes
without having to go all over the place updating type signatures. Once
I'm happy with a module's API, I can properly define it in an
interface.

The types really helped me reason about he code, and forced me to
specify corner cases I may have ignored otherwise. As a result the
code is cleaner this time.


## What did I miss?

### Interactive coding
OCaml has its own REPL, UTop. It works well, and has Dune integration,
so it will find all the sources for the project. Sadly I couldn't make
it work from Emacs. I did try it a bit from the console, but since it
was a bit more out of the way I didn't use it that much.

I missed the real time conversation between Sly/Slime and my code, and
being able to test my functions immediately after writing them. I will
have to investigate a bit more!

### Structured editing
Editing S-Expressions through paredit is quite a nice
experience. Being able to move around the code, change the scope of
the expresions, and move parens around is very convenient, and it's a
bit sad going back to do all those changes by hand.

## And what about the code?
Well, I'm not even half way through [Real World
OCaml](https://dev.realworldocaml.org/toc.html), so I barely know what
I'm doing, but this is what I came up with.

### The Store
In the same way I did for the Common Lisp version, the store is just a
hash table. In Cledis I had to use locks to ensure two different
threads don't try to update it at the same time, but in Eio I was able
to handle the communication through a Stream. I have a single handler
processing the queries from a single Fiber, so all the access is safe
and serialized.

It approaches an actor model, so adding things like sharding should be
fairly intuitive. More things to investigate!

```ocaml
open! Base
open Eio.Std

type 'v command =
  | Set of string * 'v
  | Get of string * ('v option Promise.u)

type 'v t = 'v command Eio.Stream.t

let make sw =
  let stream = Eio.Stream.create 120 in
  let table = Hashtbl.create (module String) in
  let rec handler () =
    match Eio.Stream.take stream with
    | Set (key, value) ->
       let _ = Hashtbl.add table ~key ~data:value in
       handler ()
    | Get (key, resolver) ->
       let value = Hashtbl.find table key in
       Promise.resolve resolver value;
       handler ()
  in
  Fiber.fork ~sw handler;
  stream

let set store key value =
  Eio.Stream.add store (Set (key, value))

let get store key =
  let promise, resolver  = Promise.create () in
  Eio.Stream.add store (Get (key, resolver));
  Promise.await promise
```

### The protocol
This is pretty much equivalent to the old version, but now we have
clearly defined types for commands and responses:

```ocaml
type value =
  | Num of int32
  | String of string
[@@deriving sexp]

type command =
  | Set of string * value
  | Get of string
[@@deriving sexp]

type response =
  | All_ok 
  | Done of string * value
  | Error of string
[@@deriving sexp]
```

And a couple of functions to send and receive lists of them over the
wire:

```ocaml
let get_sexp_list of_sexp reader =
  let len = Bytes.of_string @@ Eio.Buf_read.take 4 reader in
  let len = Int.of_int32_exn (Stdlib.Bytes.get_int32_be len 0) in
  let msg = Eio.Buf_read.take len reader in
  let sexp = Parsexp.Single.parse_string_exn msg in
  of_sexp sexp

let send_sexp_list of_sexp objs writer =
  let sexp = of_sexp objs in
  let sexp = Sexplib.Sexp.to_string sexp in
  let len = String.length sexp in
  let buffer = Bytes.create (len + 4) in
  (** Not sure if there is a way to write into the socket without allocating buffers *)
  let _ = Encoding.push_str_exn 0 buffer sexp in
  Eio.Buf_write.bytes writer buffer
```

And the specialized versions:
```ocaml
let get_responses reader =
  get_sexp_list [%of_sexp: response list] reader

let send_responses responses writer =
  send_sexp_list [%sexp_of: response list] responses writer 

let get_commands reader =
  get_sexp_list [%of_sexp: command list]  reader

let send_commands commands writer =
  send_sexp_list [%sexp_of: command list] commands writer
```

### The Server & Client
And here we just use Eio to make client and server talk.

The server:

```ocaml
let handle_client store flow _addr =
  traceln "[SERVER] Got a connection";
  let open Kvlib.Protocol in
  let from_client = Eio.Buf_read.of_flow flow ~max_size:4096 in
  Eio.Buf_write.with_flow flow @@ fun to_client ->
                                  let query = get_commands from_client in
                                  let query_str = Sexplib.Sexp.to_string_hum ([%sexp_of: command list] query) in
                                  traceln "[SERVER] Query: %s" query_str;
                                  let response = List.map ~f:(fun cmd -> run_command store cmd) query in
                                  send_responses response to_client


let run_server socket store =
  Eio.Net.run_server socket (handle_client store)
    ~on_error:(traceln "Error found: %a" Fmt.exn)


let server ~net ~addr =
  Switch.run ~name:"server" @@ fun sw ->
                               let store = Store.make sw in
                               traceln "Store ready";
                               let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
                               traceln "Server ready.";
                               Fiber.fork ~sw (fun () -> run_server socket store)
```

And finally, the client:

```ocaml
let run_client ~net ~addr =
  Switch.run @@ fun sw ->
                let flow = Eio.Net.connect ~sw net addr in
                let open Kvlib.Protocol in
                let from_server = Eio.Buf_read.of_flow flow ~max_size:4096 in
                Eio.Buf_write.with_flow flow @@ fun to_server ->
                                                let query = [ Set ("one", Num 2l); Get "one" ] in
                                                send_commands query to_server;
                                                let response = get_responses from_server in
                                                let response = Sexplib.Sexp.to_string_hum ([%sexp_of: response list] response) in
                                                traceln "[CLIENT] Response: %s" response
                                                

let client ~net ~addr =
  Switch.run @@ fun _ ->
                traceln "[CLIENT]: Starting";
                run_client ~net ~addr

let () =
  Eio_main.run @@ fun env ->
                  client ~net:(Eio.Stdenv.net env)
                  ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 12342))
```

## Conclusion
I still have a lot to learn about OCaml, but I was able to get a
working server on just a few hours, and the experience was quite
enjoyable. Also, having the type checker covering my back was nice. 

I have to admit that I came a bit worried about functional programming
making the day to day coding too constraining, but, maybe because I
don't know enough to make my life difficult, I felt fairly
productive. A lot of the above code relies on mutation, and maybe
there are more functional ways to do it, but I didn't feel that I was
fighting the language to get here, which is a win in my mind.

There are still many things to learn, so I think I will continue
playing with Ocaml a bit longer!
