+++
title = "A basic Write Ahead Log"
date = 2025-03-30T12:05:49+02:00
images = []
tags = ["experiment", "data bases"]
categories = ["OCaml"]
draft = false
+++

This weekend I decided to add some basic persistence to my [Key-Value
store](https://github.com/jagg/ocledis). I considered going directly
into B-Trees or LSM, but they are quite involved. Moving, forward I
want to focus more on the distributed side of the store rather than low
level storage details (saving those for later!), so for now I decided
to implement something simpler, a basic Write Ahead Log (WAL, for short).

## What is a Write Ahead Log?
This
[post](https://www.architecture-weekly.com/p/the-write-ahead-log-a-foundation)
from Oskar Dudycz does a much better job than I could explaining the
theory, so I will just give the high level summary.

When a database receives an update (a new 'set' operation), it has to
store that into its memory representation of the store, which would
eventually be backed in persistent disk storage. It will also
replicate the data across other shards. If the database fails in the
middle of those operations, the data could be lost, of leave the
system in an inconsistent state!

So how do we solve this problem? We just keep a file were we append
all our updates sequentially. Appending to a file is simple and quick,
and if we do this before acknowleding the update to the client, we
will end up with a sequence of operations safely (well,
[maybe](https://notes.eatonphil.com/2025-03-27-things-that-go-wrong-with-disk-io.html))
store in disk that will help us reconstruct the state of our store in
the case of a crash.

## A forever growing log?
Well, it doesn't have to be, you can save checkpoints! When the WAL
goes over a threshold (it could be size, and/or time), we just take a
snapshot of the data in the store and save it into a checkpoint. Once
it's clear, we can truncate the WAL.

## My implementation
For this version I went with a very naive approach. All the updates to
the store are currently serialized, so I know there is only one thread
writing to the WAL, making it safe for me to do the checkpointing and
WAL truncating at any point.

Also, I'm not paying much attention to performance, opening and
closing files all the time. I've never done any benchmarking or
performance testing in OCaml, so I'm saving that for a future post!

### The Storage Interface
First I extracted the storage backend for the KV into an interface
(before I was using a Hashtbl directly):

```ocaml
open! Base

type t

val create :  unit -> t
val put : t -> key:Model.Key.t -> value:Model.value -> unit 
val get : t -> key:Model.Key.t -> Model.value option
val iter : t -> f:(key:Model.Key.t -> data:Model.value -> unit) -> unit
```

And moved my Hashtbl implementation into its own module:

```ocaml
open! Base

type t = (Model.Key.t, Model.value) Hashtbl.t

let create () = Hashtbl.create (module Model.Key) 

let put table ~key ~value =
  Hashtbl.set table ~key ~data:value

let get table ~key =
  Hashtbl.find table key

let iter table ~f =
  Hashtbl.iteri table ~f
```

Now I can follow the same pattern for the WAL, and swap
implementations later. This is the main type:

```ocaml
type t = {
  table : Storage_hashtbl.t;
  (** The path to the file storing the write ahead log *)
  wal_path : string;
  (** The path to the file storing the checkpoint *)
  checkpoint_path : string;
  (** The number of operations we have so far, to trigger the checkpoint save*)
  mutable operations : int;
}
```

Note that for storage we use the Storage_hashtbl module we defined
above, we keep the same Hashtbl implementation at the core of the new
module!.

The store currently supports two different types, int32 and strings,
for both values and keys. We can represent each one in the same way,
in both WAL, and checkpoint file:

- A first byte for the type: 0 for int32, 1 for string
- If it's an int32, the next 4 bytes contain the value
- If it's a string, the next 4 bytes contain the size, and the next n,
  the value of the string.
  
```ocaml
let store_value channel = function
  | Model.Num n ->
    (** The type of int32 is 0 *)
    Out_channel.output_byte channel 0;
    let buffer = Bytes.create 4 in
    let _ = Encoding.push_int32_exn 0 buffer n in
    Out_channel.output channel buffer 0 4
  | Model.String s ->
    (** The type of string is 1 *)
    Out_channel.output_byte channel 1;
    let len = String.length s in
    let buffer = Bytes.create (4 + len) in
    let _ = Encoding.push_str_exn 0 buffer s in
    Out_channel.output channel buffer 0 (Bytes.length buffer)
```
This means that we can use the same function to load both WAL and Checkpoint:

```ocaml
let load_data table path =
  In_channel.with_open_gen [Open_binary] 0o666 path
    (fun channel ->
       let rec read () =
         match In_channel.input_byte channel with
         | None -> () (** EOF *)
         | Some input_type ->
           let key = match input_type with
             | 0 -> Model.Key.Num (get_num channel)
             | 1 -> Model.Key.String (get_str channel)
             | _ -> raise (Failure "Type not Supported!")
           in
           match In_channel.input_byte channel with
           | None -> () (** EOF *)
           | Some input_type ->
             let value = match input_type with
               | 0 -> Model.Num (get_num channel)
               | 1 -> Model.String (get_str channel)
               | _ -> raise (Failure "Type not Supported!")
             in
             let _ = Storage_hashtbl.put table.table ~key ~value in
             read ()
       in
       read ()
    )
```

We also have a function to save the checkpoint on demand:

```ocaml
let save_checkpoint table =
  Out_channel.with_open_gen [Open_binary; Open_creat; Open_trunc; Open_append] 0o666 table.checkpoint_path
    (fun out ->
       Storage_hashtbl.iter table.table ~f:(fun ~key ~data:value ->
           store_key out key;
           store_value out value;
           Out_channel.flush out;
         );
    )
```

And we all this happens when we get updates. First we add one to our
operation count, and append to the WAL. If our count goes over the
limit, we trigger the checkpoint, persisting all our data and
truncating the WAL.

```ocaml
let put table ~key ~value =
  table.operations <- table.operations + 1;
  Out_channel.with_open_gen [Open_binary; Open_creat; Open_append] 0o666 table.wal_path
    (fun wal ->
       store_key wal key;
       store_value wal value;
       Out_channel.flush wal;
       Storage_hashtbl.put table.table ~key ~value;
    );
  if table.operations > max_wal_size then
    let _ = save_checkpoint table in
    Stdlib.Sys.remove table.wal_path;
    table.operations <- 0;
```

We also have to make sure we load the WAL during start up:

```ocaml
let create () =
  let table = {
    table = Storage_hashtbl.create ();
    wal_path = "./storage.bin";
    checkpoint_path = "./checkpoint.bin";
    operations = 0;
  } in
  if Stdlib.Sys.file_exists table.checkpoint_path then load_checkpoint table;
  if Stdlib.Sys.file_exists table.wal_path then
    let _ = load_wal table in
    save_checkpoint table; 
    Stdlib.Sys.remove table.wal_path;
  else ();
  table
```

### Is it still quick?
No. 

As you can see in the code above, we are opening and flushing the
file for every write and read operation. In a real life system we
would find a way to keep the file open for longer, maybe batch
writes. But this is functional, and provides a baseline to compare
performance, so should be a decent starting point, I think.

So, I wrote a quick benchmark to see where we stand today. Let's run a
1000 sets and gets, using the raw Hashtbl and the WAL backed one, and
see what's the price of safety (at least naive safety)!

```ocaml
open! Core
open! Core_bench

(** dune exec -- ./bench.exe -ascii -quota 0.25 *) 
let amount = 1000
let value_pairs =
  List.init amount ~f:(fun i -> ("key" ^ string_of_int i, Int32.of_int_exn i))

module type Storage = sig
  type t

  val create :  unit -> t
  val put : t -> key:Kvlib.Model.Key.t -> value:Kvlib.Model.value -> unit 
  val get : t -> key:Kvlib.Model.Key.t -> Kvlib.Model.value option
end

module Bench (S : Storage) = struct
  let run_bench () =
    let table = S.create () in
    List.iter value_pairs
      ~f:(fun (key, value) ->
          let key = Kvlib.Model.Key.String key in
          let value = Kvlib.Model.Num value in
          S.put table ~key ~value;
          let _ = S.get table ~key in
          ()
        )
end 

module Bench_hashtbl = Bench(Kvlib.Storage_hashtbl)
module Bench_wal = Bench(Kvlib.Write_ahead_log)

let benchmarks =
  [ "Hashtable", Bench_hashtbl.run_bench
  ; "WAL", Bench_wal.run_bench]

let () =
  List.map benchmarks ~f:(fun (name, test) ->
      Core_bench.Bench.Test.create ~name test)
  |> Core_bench.Bench.make_command
  |> Command_unix.run
```

And if I run it:

```bash
$ dune exec -- ./bench.exe -ascii -quota 0.25

Entering directory '/home/tony/projects/ocaml/ocledis'
Leaving directory '/home/tony/projects/ocaml/ocledis'
Estimated testing time 500ms (2 benchmarks x 250ms). Change using '-quota'.

  Name            Time/Run    mWd/Run   mjWd/Run   Prom/Run   Percentage
 ----------- -------------- ---------- ---------- ---------- ------------
  Hashtable       248.27us    18.60kw    11.65kw    10.11kw        0.21%
  WAL         119_260.79us   809.65kw    17.89kw    16.35kw      100.00%
```

So, it seems we are now about x480 times slower...

While this is OK for this experiment, it's nowhere near an acceptable
value in a production system, so I will have to implement some
optimizations, and see if we can make that a bit better!

In the meantime, all the code is available [here](https://github.com/jagg/ocledis).
