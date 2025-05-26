+++
title = "Leader election using Raft"
date = 2025-05-26T18:53:21+02:00
images = []
tags = ["raft", "distributed systems"]
categories = ["OCaml"]
draft = true
+++

In my previous posts I've been working on a basic key-value store,
using a write-ahead log, and replicating its data across several
replicas (see posts on [WAL]({{% ref "wal" %}}), and [replication]({{%
ref "replication" %}}))). This version had a hardcoded leader handling
all writes, and sending the data to the followers, but had several
shortcomings, the main one among that was that the configuration had
to include which node is the leader, and if it dies, the cluster can't
do any work, doesn't matter how many nodes remain.

In this post I'm trying to make that a little bit better by
implementing [Raft](https://raft.github.io/raft.pdf), a leader
election algorithm designed to be easier to understand and implement
(by consensus algorithm standards, that is)

If you are coming from the previous posts, this implementation
required some many changes in the architecture that I started from
scratch in a new repository. The code is available in
[GitHub](https://github.com/jagg/raft).

