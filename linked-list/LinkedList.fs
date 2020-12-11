module LinkedList

type Node =
    { value: int
      mutable previous: Node option
      mutable next: Node option }

type LinkedList =
    { mutable head: Node option
      mutable tail: Node option }

let mkLinkedList () = { head = None; tail = None }

let addToEmpty newValue linkedList =
    let node =
        { value = newValue
          previous = None
          next = None }

    linkedList.head <- Some node
    linkedList.tail <- Some node

let pop linkedList =
    match linkedList.tail with
    | Some tail ->
        linkedList.tail <- tail.previous
        tail.previous
        |> Option.map (fun p -> p.next <- None)
        |> ignore
        tail.value
    | None -> failwith "Empty list"

let shift linkedList =
    match linkedList.head with
    | Some head ->
        linkedList.head <- head.next
        head.next
        |> Option.map (fun n -> n.previous <- None)
        |> ignore
        head.value
    | None -> failwith "Empty list"

let push newValue linkedList =
    match linkedList.tail with
    | Some tail ->
        let node =
            Some
                { value = newValue
                  previous = Some tail
                  next = None }

        tail.next <- node
        linkedList.tail <- node
    | None -> addToEmpty newValue linkedList

let unshift newValue linkedList =
    match linkedList.head with
    | Some head ->
        let node =
            Some
                { value = newValue
                  previous = None
                  next = Some head }

        head.previous <- node
        linkedList.head <- node
    | None -> addToEmpty newValue linkedList
