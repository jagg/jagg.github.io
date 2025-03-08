+++
title = "Using Zig from Common Lisp"
date = 2025-03-08T20:38:02+01:00
images = []
tags = ["low level", "CFFI"]
categories = ["common lisp", "zig"]
draft = false
+++

Last week I started playing with my own toy key-value store (see the
previous [post]({{% ref "cledis" %}})). At the end I got to a
hashtable exposed over the network, using a protocol based on
S-Expressions. For the next steps, I have two alternatives, I can work
on the low level representation of the data, maybe implement B-Trees,
and some storage, or I can go up instead, and see how can I make it
distributed, and play with some nice algorithms.

Well, I haven't made my mind yet, but I thought I may want to call
some code from C eventually, so I spent some time trying CFFI, and
since C was a bit boring, I tried Zig!

It turns out it's not that complicated, at least for simple calls. I
wrote a struct with some numbers and a pointer to a null terminated
string:

```zig
pub const Point = struct  {
    label: [*:0]const u8,
    x: i32,
    y: i32,
};
```

And a constructor and destructor set:

```zig
export fn makepoint(label: [*:0]const u8, x: i32, y: i32)
callconv(.C) *Point {
             var p = std.heap.c_allocator.create(Point) catch unreachable;
             p.label = std.heap.c_allocator.dupeZ(u8, std.mem.span(label)) catch unreachable;
             p.x = x;
             p.y = y;
             return p;
}

export fn freepoint(p: *Point)
callconv(.C) void {
             std.heap.c_allocator.free(std.mem.span(p.label));
             std.heap.c_allocator.destroy(p);
}
```

And to see if I could modify the struct, I wrote a function to
multiply the integers:

```zig
export fn multpoint(p: *const Point, n: i32, result: *Point)
callconv(.C) void {
             result.x = p.x * n;
             result.y = p.y * n;
}
```

The I just need to compile it into a library:
```bash
$ zig build-lib -dynamic --library c main.zig
```

Now, back to Common Lisp, make sure CFFI is available, load the library:

```lisp
(cffi:load-foreign-library "~/projects/lisp/experiments/libmain.so")
```

and define the struct and the functions:

```lisp
(cffi:defcstruct point
  (label :string)
  (x :int)
  (y :int))

(cffi:defcfun "makepoint" :pointer
  (label :string)
  (x :int)
  (y :int))

(cffi:defcfun "freepoint" :void
  (p :pointer))

(cffi:defcfun "multpoint" :void
  (p :pointer)
  (n :int)
  (result :string))
```

And to see if everything works, let's use them:

```lisp
(defun points ()
  (let ((p (makepoint "my-vector" 10 10)))
    (multpoint p 20 p)
    (format t "point: ~a, ~a, ~a"
            (cffi:foreign-slot-value p '(:struct point) 'label)
            (cffi:foreign-slot-value p '(:struct point) 'x)
            (cffi:foreign-slot-value p '(:struct point) 'y))
    (freepoint p)))
```

It appears to work!

```
EXPERIMENTS> (points)
point: my-vector, 200, 200
; No values
EXPERIMENTS> 
```

### Not all is clear yet
When I was experimenting with the code I realised that if I switched
the order of the fields, so that the string is the last one:

```zig
pub const Point = struct  {
    x: i32,
    y: i32,
    label: [*:0]const u8,
};
```

```lisp
(cffi:defcstruct point
  (x :int)
  (y :int)
  (label :string))
```

It does't work! It compiles without issues, but when I try to run the
code I get an error:

```
Unhandled memory fault at #xC8000000C8.
   [Condition of type SB-SYS:MEMORY-FAULT-ERROR]
```

I don't know why that could be, I assume it must be some problem with
memory alignment, but in theory the code should have been equivalent...

If anybody happens to read this and knows the answer, please ping me
on Twitter or Bluesky, I would love to know what's going on!
