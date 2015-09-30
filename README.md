# Simple FIFO for Common Lisp

Simple implementation of a [queue](https://en.wikipedia.org/wiki/Queue_%28abstract_data_type%29) for Common Lisp.

There is nothing special or difficult going on here. I just find myself using queues a lot and disliked having to recode the same functions over and over again.

## Quickstart

To create a new heap, use the `make-queue` function.

    (make-queue &key initial-contents)

Once you have a queue, simply use `queue-push` and `queue-pop` to enqueue and dequeue items from it.

    CL-USER > (make-queue :initial-contents '(1 2 3))
    #<QUEUE (1 2 3)>

    CL-USER > (queue-pop *)
    1

    CL-USER > (queue-push 'hello **)
    HELLO

    CL-USER > (queue-pop ***)
    2
