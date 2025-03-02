+++
title = "Redis from scratch, or Cledis"
date = 2025-03-02T12:46:38+01:00
images = []
tags = ["networking", "async"]
categories = ["common lisp"]
draft = false
+++

This week I was looking for something new, and I realised that I've
never tried to do any network programming using Common Lisp. Also,
some time ago I came across the [Build your own
Redis](https://build-your-own.org/redis/#table-of-contents) book, but
never tried to go through it. The project is perfect, it contains
threading, networking, and even some async programming. I had no idea
what the Common Lisp ecosystem for those things looks like, so it was
time to find out.

So far I'm still up to the first part, building a basic key value
store. The book is quite detailed, and it's all C++, so a lot of the
complexity goes away just by using CL. That said, I'm trying to stay
close to the spirit of the original code.

I did some research, and I found that there are bindings for libuv
([cl-async](https://orthecreedence.github.io/cl-async/)), and it's
well documented and easy to use!. I also found
[usocket](https://github.com/usocket/usocket) for plain sockets, which
I tried to use for the client, but it seems the socket abstraction
they use is not compatible, and I wanted to use the same set of
encoding/decoding functions in both client and server, so I ended up
using cl-async also for the client.

#### A Key Value Store
In this first version I'm building a naive key value store, just a
hash-map protected by a lock, with two operations, `set` and `get`.

```lisp
(defvar *store* (make-hash-table :test 'equal))
(defvar *lock* (bt:make-lock))

(defun put-value (key val)
  (bt:with-lock-held (*lock*)
    (setf (gethash key *store*) val)))

(defun get-value (key)
  (bt:with-lock-held (*lock*)
    (gethash key *store*)))
```

This would obviously create a lot of contention in the store, since it
means we will serialize all incoming requests, but we can think about
improving that later.

#### Message Encoding
The next step is thinking about how to serialize the operations, so that
clients can tell the server what they need.

The encoding the book uses allows chaining several operations in the
same message. The first four bytes of the result buffer encode the
total length of the message, and then appends operations (get or set),
following the same pattern: a size, and the bytes representing ASCII
for each operation:

[ Total Size / Op1 Size / Op1 Bytes / Op2 Size / Op2 Bytes ]

This approach allows the server to execute the operations as they are
parsed (Op1 arrives, execute on a thread, and go back to reading). The
alternative would be to wait for all the input to arrive, then parse
it all, and then execute the full set.

On my first attempt I was more worried about the details of the
communication than optimising the protocol, so I chose to take
advantage of S-Expressions to handle the parsing for me. This means
that I send them all in one batch:

[ Total Size / Ops List ]

The chain of operations is represented by a list of symbols:

```lisp
'((set one 1) (get one) (set one 23))
```

I can read a string containing those values with `read-from-string`
and get S-Expressions with my operations, and I can transform them
into a string again using `princ-to-string`.

```lisp
(defun naive-encode-msg (ops)
  "Encode a list of operations into bytes. The input looks like this: '((set one 1) (get two))"
  (let* ((vec (make-array (* (length ops) 30) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
         (str (princ-to-string ops))) 
    (push-str vec str)
    (let ((buffer (make-array (length vec) :element-type '(unsigned-byte 8))))
     (dotimes (i (length vec) buffer)
       (setf (aref buffer i) (aref vec i))))))

(defun naive-decode-msg (buffer stream)
  "Encode a list of operations into bytes. The input looks like this: '((set one 1) (get two))"
  (let* ((len (rec-int buffer stream))
         (str (rec-payload buffer stream len)))
    (read-from-string str)))
```

Then I just need functions to serialize numbers and strings:

```lisp
(defun push-num (vec n)
  (let ((bytes (uint32-to-be-bytes n)))
    (dotimes (i (length bytes))
      (vector-push-extend (aref bytes i) vec (+ 1 (length vec))))))

(defun push-str (vec str)
  (let ((bytes (str-coerce-bytes str)))
    (push-num vec (length bytes))
    (dotimes (i (length bytes))
      (vector-push-extend (aref bytes i) vec (+ 1 (length vec))))))
```

And when I get the list of operations, I can execute them all:

```lisp
(defun run-ops (ops)
  (dolist (op ops)
    (case (car op)
      (set (key-value:put-value (cadr op) (caddr op)))
      (get (format t "Value = ~a~%" (key-value:get-value (cadr op)))))))
```

#### The Server and Client
Now we just need to make these functions accessible through the
protocol. We create a TCP server listening on our port of choice,
enabling streaming, so that we can consume the socket on demand. Up
starting the server just waits for requests, parses the input and runs
the operations.

```lisp
(defun server (port)
  (setf *server* 
        (cl-async:tcp-server nil port
                             (lambda (socket stream)
                               (format t "[SERVER] Ready to read~%")
                               (let* ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
                                      (ops (naive-decode-msg buffer stream)))
                                 (format t "[SERVER] This is the message: ~a~%" ops)
                                 (run-ops ops))
                               (send-msg "Done!" socket)
                               (format t "[SERVER] That was it.~%"))
                             :event-cb (lambda (err) (format t "[SERVER] Event: ~a~%" err))
                             :stream t))
  (cl-async:signal-handler 2 (lambda (sig)
                               (declare (ignore sig))
                               (cl-async:exit-event-loop))))
```

And the client will just connect, send a few operations, and wait for
a response, the close itself:

```lisp
(defun client (port)
  (as:tcp-connect "127.0.0.1" port
    (lambda (socket stream)
         (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
           (format t "[CLIENT] Respose: ~a~%" (rec-msg buffer stream))
           (as:exit-event-loop)))
    :event-cb (lambda (event) (format t "[CLIENT] Event received: ~a~%" event))
    :stream t
    :data (naive-encode-msg '((SET one 1) (GET one)))
    :read-timeout 5))
```

And now to test the whole thing:

```
CLEDIS> (launch-async-server 12342)
#<SB-THREAD:THREAD "Server" RUNNING {1004CB3243}>
CLEDIS> (launch-client 12342)
#<SB-THREAD:THREAD "Client" RUNNING {1004CB70C3}>
[SERVER] Ready to read
[SERVER] This is the message: ((SET ONE 1) (GET ONE))
Value = 1
[SERVER] That was it.
[CLIENT] Respose: Done!
[SERVER] Event: #<SOCKET-EOF #<TCP-SOCKET {10054C8093}>: 0: NIL {1001E5CD63}>
CLEDIS> 
```

I'm keeping the code in [GitHub](https://github.com/jagg/cledis/tree/master).

#### Next steps
At this stage this is barely a toy, the protocol can be optimised, we
should probably use some sort of thread-pool to execute the
operations, and I'm sure there are better ways to do the
serialization, but this is not too bad for a few hours tinkering.

Writing the code was actually quite nice, the libraries have good
documentation, but I need to investigate a bit more about what
cl-async actually does under the hood, I'm not convinced the code I
wrote is completely right.

I will follow up with the next steps of the book, and at the end I
will try some profiling, the CL Cookbook has a section about it, so I
would like to try it myself!
