+++
title = "Base64 Encoding & Decoding"
date = 2025-02-22T18:23:44+01:00
images = []
tags = ["low level"]
categories = ["common lisp"]
draft = true
+++

Looking for a first project to play with, I remembered
[CryptoPals](https://cryptopals.com). I did some of it several years
ago, and it was fun, so I thought it could be a good way to get
familiar with Common Lisp, at least the first set. With that in mind,
I had a look, and hear we are!

The very first challenge is about hex and base64 encoding. The first
time around I just used available libraries for both (I used Rust at
the time), but the point of this was to open some black boxes, so I
decided to implement everything from scratch!

I've used Base64 encoding several times in the past, both in my
personal projects, and during my day job, and while I understood
conceptually what it was, I had never tried to implement it
myself. After a quick search I found a very nice description of the
algorithm at
[b64encode.com](https://b64encode.com/blog/base64-algorithm/), I
opened Emacs, and got to work.

The algorithm is very well explained in the link above, so I will not
try to do it again, but this is what I came up with for the encoding part:

```lisp
(defun get-n-bits (n byte pos)
  "Get n bits from byte, starting at pos"
  (when byte
    (ldb (byte n pos) byte)))

(defun combine-3 (bytes start end)
  "Combine an array of three bytes into a single integer"
  (let ((seq (subseq bytes start (min (length bytes) end))))
    (values 
     (when seq 
       (logior (ash (try-aref seq 0) 16)
               (ash (try-aref seq 1) 8)
               (ash (try-aref seq 2) 0)))
     (+ 1 (- (min (length bytes) end) start)))))

(defun bytes-to-b64 (bytes)
  "Base64 Encode an array of bytes into a string"
  (with-output-to-string (str)
    (do* ((start 0 (+ start 3))
          (end 3 (+ end 3)))
         ((>= start (length bytes)) str)
      (multiple-value-bind (combined byte-count) (combine-3 bytes start end)
        (dotimes (n byte-count)
          (princ (char *b64-lookup* (get-n-bits 6 combined (- 18 (* n 6)))) str))
        (dotimes (n (- 4 byte-count))
          (princ #\= str))))))


```

It was much shorter than I thought it would be, so I was quite
pleased! I was suprised with how handy the `ldb` function was, and how
intuitive was to write to a string as a stream.

You can find the full source [here](https://github.com/jagg/crypto-pals/blob/master/src/encodings.lisp).

After writing the code I went around to see how other people
implemented this, and I found
[this](https://github.com/massung/base64/blob/master/base64.lisp),
written 10 years ago. It's a lot nicer than my version, and I love how
they also used the input string as a stream, calling `read-char`, that
simplifies things. I'm still not sold on the `loop` macro. Seems easy
to read, but I find it a bit too magical, and unintuitive to
write. But I guess that's because I'm not familiar with it. I'll have
to try to use it in the future!
