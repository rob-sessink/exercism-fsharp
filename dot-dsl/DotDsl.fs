module DotDsl

type Attribute = string * string
type Node = string * Attribute list
type Edge = string * string * Attribute list

type Entry =
    | Attribute of Attribute
    | Node of Node
    | Edge of Edge

type Graph = Entry list

let graph children = children

let attr key value : Entry = Attribute(key, value)

let node key attrs = Node(key, attrs)

let edge left right attrs = Edge(left, right, attrs)

let isAttr =
    function
    | Attribute _ -> true
    | _ -> false

let isNode =
    function
    | Node _ -> true
    | _ -> false

let isEdge =
    function
    | Edge _ -> true
    | _ -> false

let attrs graph =
    graph |> List.filter isAttr |> List.sort

let nodes graph =
    graph |> List.filter isNode |> List.sort

let edges graph =
    graph |> List.filter isEdge |> List.sort
