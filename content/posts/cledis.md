+++
title = "Redis from scratch, or Cledis"
date = 2025-03-02T12:46:38+01:00
images = []
tags = ["networking", "async"]
categories = ["common lisp"]
draft = true
+++

This week I was looking for something new, and I realised that I've
never tried to do any network programming using Common Lisp. Also,
some time ago I came across the [Build your own
Redis](https://build-your-own.org/redis/#table-of-contents) book, but
never tried to go through it. The project is perfect, it contains
threading, networking, and even some async programming. I had no idea
what the Common Lisp ecosystem for those things looks like, so it's
time to find out.

So far I'm still up to the first part. The book is quite detailed, and
it's all C++, so a lot of the complexity goes away just by using
CL. That said, I'm trying to stay close to the spirit of the original
code. 

I did some research, and I found that there are bindings for libuv
([cl-async](https://orthecreedence.github.io/cl-async/)), and it's
well documented and easy to use!. I also found
[usocket](https://github.com/usocket/usocket) for plain sockets, which
I tried to use for the client, but it seems the socket abstraction
they use is not compatible, and I wanted to use the same set of
encoding/decoding functions in both client and server.

#### Message Encoding for our Key / Value store
The encoding the book uses allows chaining several operations in the
same message. The first four bytes of the result buffer encode the
total length of the message, and then appends operations (get or set),
following the same patter, a size, and the bytes for a string message.

This approach allows the server to execute the operations as they
parse, instead of having to wait for all the input to arrive, then
parse it all, and then execute.

On my first attempt I was more worried about the details of the
communication, so I chose to take advantage of S-Expressions to handle
the parsing for me. My chain of operations is represented by a list:

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
