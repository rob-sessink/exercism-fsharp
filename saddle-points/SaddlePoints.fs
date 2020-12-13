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

let greatest array = elements List.sortDescending array

let smallest array = elements List.sort array

let smallestInColumn (matrix: int [,]) colIndex = matrix.[*, colIndex] |> smallest

let filterByIndex index entries =
    entries |> List.filter (fun c -> fst c = index)

let createSaddle rowPosition colPosition =
    (fst colPosition + 1, fst rowPosition + 1)

let findSaddlePoints matrix =
    seq {
        for row in 0 .. (Array2D.length1 matrix) - 1 do
            yield! matrix.[row, *]
                   |> greatest
                   |> List.map (fun rowPosition ->
                       smallestInColumn matrix (fst rowPosition)
                       |> filterByIndex row
                       |> List.map (createSaddle rowPosition))
                   |> List.concat
    }
    |> Seq.toList

let saddlePoints (matrix: int list list) =
    match matrix with
    | [ [] ] -> []
    | _ -> findSaddlePoints (array2D matrix)
