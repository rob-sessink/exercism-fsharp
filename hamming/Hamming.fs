module Hamming

let equalPair (letter1: char) (letter2: char): int = if letter1 <> letter2 then 1 else 0

let distance (strand1: string) (strand2: string): int option =
    let distance =
        match strand1.Length = strand2.Length with
        | false -> None
        | true ->
            Seq.map2 equalPair strand1 strand2
            |> Seq.sum
            |> Some

    distance
