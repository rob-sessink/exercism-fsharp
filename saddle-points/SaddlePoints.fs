module SaddlePoints

let getElements minOrMax array =
    array
    |> List.ofArray
    |> List.indexed
    |> List.filter (fun e -> (snd e) = (minOrMax array))

let smallestInColumn (matrix: int [,]) colIndex =
    matrix.[*, colIndex] |> getElements Array.min

let filterByIndex index entries =
    entries |> List.filter (fun c -> fst c = index)

let createSaddle rowPosition colPosition =
    (fst colPosition + 1, fst rowPosition + 1)

let saddlePoints (matrix: int list list) =
    let a2d = array2D matrix
    seq {
        for row in 0 .. (Array2D.length1 a2d) - 1 do
            yield! a2d.[row, *]
                   |> getElements Array.max
                   |> List.map (fun rowPosition ->
                       smallestInColumn a2d (fst rowPosition)
                       |> filterByIndex row
                       |> List.map (createSaddle rowPosition))
                   |> List.concat
    }
    |> Seq.toList
