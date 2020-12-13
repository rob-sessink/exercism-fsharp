module SaddlePoints

let elements sorterFunc array =
    array
    |> List.ofArray
    |> List.indexed
    |> List.groupBy snd
    |> sorterFunc
    |> List.take 1
    |> List.map snd
    |> List.concat

// get greatest entries in an array returned as indexed values list
let greatest array = elements List.sortDescending array

// get smallest entries in an array returned as indexed values list
let smallest array = elements List.sort array

let column (a2d: int [,]) coli = a2d.[*, coli]

// find smallest entries for a column in matrix
let smallestForColumn matrix colIndex =
    let column = column matrix colIndex
    smallest column

// filter a list of entry by index (row or column)
let filterByIndex row entries =
    entries |> List.filter (fun c -> fst c = row)

// determine if row/column combination is a saddle by comparison of row and column values, if equal return SaddlePoint
let isSaddle rowPosition colPosition =
    if (snd rowPosition) = (snd colPosition)
    then Some(fst colPosition + 1, fst rowPosition + 1)
    else None

let chooseSaddles row columns =
    List.choose (fun column -> isSaddle row column) columns

let findSaddlePoints matrix =
    let rows = (Array2D.length1 matrix) - 1
    seq {
        for row in 0 .. rows do
            yield! matrix.[row, *]
                   |> greatest
                   |> List.map (fun rowPosition ->
                       smallestForColumn matrix (fst rowPosition)
                       |> filterByIndex row
                       |> chooseSaddles rowPosition)
                   |> List.concat
    }
    |> Seq.toList

let saddlePoints (matrix: int list list) =
    match matrix with
    | [ [] ] -> []
    | _ -> findSaddlePoints (array2D matrix)
