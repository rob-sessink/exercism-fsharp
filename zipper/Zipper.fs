module Zipper

// http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/

// (tree 1 (subTree 2 None (leaf 3)) (leaf 4))
//            1            1 = L(BT(1, T2, T4) T(N, N, N)
//          /   \
//         2     4         2 = L(BT(2, N, T3)  LP(T, 1, T4))
//        / \   / \        4 = L(BT(4, N, N)   RP(T, 1, T2))
//       N   3 N   N       3 = L(BT(3, N, N)   RP(2, 2, N))

type BTree = BTree of value: int * left: BTree option * right: BTree option

let tree value left right = BTree(value, left, right)

type Path =
    | Top
    | Left of parentPath: Path * parentValue: int * rightSiblings: BTree option
    | Right of parentPath: Path * parentValue: int * leftSiblings: BTree option

type Location = Location of focus: BTree * path: Path

let fromTree (tree: BTree) = Location(tree, Top)

let toTree (location: Location): BTree =
    let rec upTree path (tree: BTree) =
        match path with
        | Top -> tree
        | Left (parentPath, parentValue, rightSiblings) ->
            upTree parentPath (BTree(parentValue, Some tree, rightSiblings))
        | Right (parentPath, parentValue, leftSiblings) ->
            upTree parentPath (BTree(parentValue, leftSiblings, Some tree))

    let (Location (tree, path)) = location
    upTree path tree

let left (location: Location) =
    let (Location (BTree (value, left, right), path)) = location
    match left with
    | Some l -> Location(l, Left(path, value, right)) |> Some
    | None -> None

let right (location: Location) =
    let (Location (BTree (value, left, right), path)) = location
    match right with
    | Some r -> Location(r, Right(path, value, left)) |> Some
    | None -> None

let value (location: Location) =
    let (Location (BTree (value, left, right), path)) = location
    value

let up (location: Location) =
    let (Location (tree, path)) = location
    match path with
    | Top -> None
    | Left (parentPath, parentValue, rightSiblings) ->
        Location(BTree(parentValue, Some tree, rightSiblings), parentPath)
        |> Some
    | Right (parentPath, parentValue, leftSiblings) ->
        Location(BTree(parentValue, leftSiblings, Some tree), parentPath)
        |> Some

let setValue value (location: Location) =
    let (Location (BTree (current, left, right), path)) = location
    (Location(BTree(value, left, right), path))

let setLeft subtree (location: Location) =
    let (Location (BTree (value, left, right), path)) = location
    Location(BTree(value, subtree, right), path)

let setRight subtree (location: Location) =
    let (Location (BTree (value, left, right), path)) = location
    Location(BTree(value, left, subtree), path)
