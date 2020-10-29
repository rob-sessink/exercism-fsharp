module BinarySearchTree

type Node =
    { Data: int
      Left: Node option
      Right: Node option }

let rec add (node: Node option) data =
    match node, data with
    | (Some n, d) when d <= n.Data -> { n with Left = Some(add n.Left d) }
    | (Some n, d) when d >= n.Data -> { n with Right = Some(add n.Right d) }
    | _, d -> { Data = d; Left = None; Right = None }

let left node = node.Left

let right node = node.Right

let data node = node.Data

let create items =
    items
    |> List.fold (fun acc item -> Some(add acc item)) None
    |> Option.get

let sortedData node =
    let rec dfs node =
        match node with
        | None -> []
        | Some (n) -> (dfs n.Left) @ [ n.Data ] @ (dfs n.Right)

    Some(node) |> dfs

//   2
//  / \
// 1   3
//      \
//       6
//      / \
//     5   7
