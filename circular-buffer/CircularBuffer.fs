module CircularBuffer

open System

type CircularBuffer<'a> = CircularBuffer of head: int * tail: int * full: bool * size: int * entries: 'a []

let increment pointer size = (pointer + 1) % size

let decrement pointer size = (pointer + size - 1) % size

let isEmpty buffer =
    let (CircularBuffer(head, tail, full, _, _)) = buffer
    head = tail && full = false

let mkCircularBuffer size = CircularBuffer(0, 0, false, size, Array.zeroCreate size)

let clear buffer =
    let (CircularBuffer(_, _, _, size, _)) = buffer
    mkCircularBuffer size

let write value buffer =
    let (CircularBuffer(head, tail, full, size, entries)) = buffer
    match full with
    | true -> raise (Exception("Buffer is full"))
    | false ->
        let incTail = increment tail size
        let isFull = (incTail = head)
        Array.set entries (tail) value
        (CircularBuffer(head, incTail, isFull, size, entries))

let forceWrite value buffer =
    let (CircularBuffer(head, tail, full, size, entries)) = buffer
    match full with
    | false -> write value buffer
    | true ->
        let incHead = increment head size
        let incTail = increment tail size
        Array.set entries (head) value
        (CircularBuffer(incHead, incTail, full, size, entries))

let read buffer =
    let (CircularBuffer(head, tail, _, size, entries)) = buffer
    match isEmpty buffer with
    | true -> raise (Exception("Buffer is empty"))
    | false ->
        let incHead = increment head size
        let data = entries.[head]
        (data, (CircularBuffer(incHead, tail, false, size, entries)))

let iter func buffer =
    let (CircularBuffer(head, _, _, _, _)) = buffer
    match isEmpty buffer with
    | true -> raise (Exception("Buffer is empty"))
    | false ->
        let rec iter cnt func buffer =
            let (CircularBuffer(_, tail, _, size, entries)) = buffer
            if cnt <> tail then
                func entries.[cnt]
                iter (increment cnt size) func buffer

        iter head func buffer

let peek buffer =
    let (CircularBuffer(head, _, _, _, entries)) = buffer
    match isEmpty buffer with
    | true -> raise (Exception("Buffer is empty"))
    | false -> entries.[head]

let length buffer =
    let (CircularBuffer(head, tail, full, size, _)) = buffer
    match isEmpty buffer with
    | true -> raise (Exception("Buffer is empty"))
    | false ->
        match buffer with
        | _ when full -> size
        | _ when tail > head -> tail - head
        | _ ->
            let count = size - head
            if tail > 0 then count + tail - 1 else count
