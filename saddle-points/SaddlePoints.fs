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

let smallestForColumn (matrix: int [,]) colIndex = matrix.[*, colIndex] |> smallest

let filterByIndex row entries = entries |> List.filter (fun c -> fst c = row)

let isSaddle rowPosition colPosition =
    if (snd rowPosition) = (snd colPosition)
    then Some(fst colPosition + 1, fst rowPosition + 1)
    else None

let chooseSaddles row columns = List.choose (fun column -> isSaddle row column) columns

let findSaddlePoints matrix =
    seq {
        for row in 0 .. (Array2D.length1 matrix) - 1 do
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
