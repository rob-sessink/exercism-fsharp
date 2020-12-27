module CircularBuffer

open System

type CircularBuffer<'a> =
    { Head: int
      Tail: int
      Capacity: int
      Size: int
      Entries: 'a array }

    static member create<'a> size =
        { Head = 0
          Tail = 0
          Capacity = size
          Size = size
          Entries = Array.zeroCreate<'a> size }

    member this.clear() = CircularBuffer.create<'a> this.Size

    member private this.inc pointer = (pointer + 1) % this.Size

    member private this.incHead() = this.inc this.Head

    member private this.incTail() = this.inc this.Tail

    member private this.dec pointer = (pointer + this.Size - 1) % this.Size

    member private this.isEmpty = this.Head = this.Tail && this.Capacity = this.Size

    member private this.isFull = this.Capacity = 0

    member this.write value =
        match this.isFull with
        | true -> raise (Exception("Buffer is full"))
        | false ->
            Array.set this.Entries (this.Tail) value
            { this with
                  Tail = this.incTail()
                  Capacity = this.Capacity - 1 }

    member this.forceWrite value =
        match this.isFull with
        | false -> this.write value
        | true ->
            Array.set this.Entries (this.Head) value
            { this with
                  Head = this.incHead()
                  Tail = this.incTail() }

    member this.read() =
        match this.isEmpty with
        | true -> raise (Exception("Buffer is empty"))
        | false ->
            let data = this.Entries.[this.Head]
            (data,
             { this with
                   Head = this.incHead()
                   Capacity = this.Capacity + 1 })

    member this.iter func =
        match this.isEmpty with
        | true -> raise (Exception("Buffer is empty"))
        | false ->
            let rec iter cnt func =
                if cnt <> this.Tail then
                    func this.Entries.[cnt]
                    iter (this.inc cnt) func

            iter this.Head func

    member this.peek() =
        match this.isEmpty with
        | true -> raise (Exception("Buffer is empty"))
        | false -> this.Entries.[this.Head]

    member this.length() =
        match this.isEmpty with
        | true -> raise (Exception("Buffer is empty"))
        | false -> this.Size - this.Capacity

let mkCircularBuffer size = CircularBuffer.create<int> size

let clear (buffer: CircularBuffer<int>) = buffer.clear()

let write value (buffer: CircularBuffer<int>) = buffer.write value

let forceWrite value (buffer: CircularBuffer<int>) = buffer.forceWrite value

let read (buffer: CircularBuffer<int>) = buffer.read()

let iter func (buffer: CircularBuffer<int>) = buffer.iter func

let peek (buffer: CircularBuffer<int>) = buffer.peek()

let length (buffer: CircularBuffer<int>) = buffer.length()
