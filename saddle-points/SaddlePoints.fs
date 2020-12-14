module SaddlePoints

let indexedFilter fExtremum array =
    array
    |> List.ofArray
    |> List.indexed
    |> List.filter (fun e -> snd e = fExtremum array)

let lowestInColumn (matrix: int [,]) colIdx = matrix.[*, colIdx] |> indexedFilter Array.min

let createSaddle rowPos colPos = (fst colPos + 1, fst rowPos + 1)

let saddlePoints (matrix: int list list) =
    let a2d = array2D matrix
    seq {
        for rowIdx in 0 .. (Array2D.length1 a2d) - 1 do
            yield! a2d.[rowIdx, *]
                   |> indexedFilter Array.max
                   |> List.map (fun rowPos ->
                       lowestInColumn a2d (fst rowPos)
                       |> List.filter (fun c -> fst c = rowIdx)
                       |> List.map (createSaddle rowPos))
                   |> List.concat
    }
    |> Seq.toList
