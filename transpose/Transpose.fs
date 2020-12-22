module Transpose

let transpose (input: string list) =

    let transpose' (input: string list) =
        let maxLen =
            input
            |> List.maxBy (fun s -> s.Length)
            |> String.length

        // A Z#        ADE.      .EDA
        // D           ..FJ      FJ..
        // EFGH        Z.G#      #G.Z
        //  J#         #.H.      .H.#
        let fill cnt (eIdx: int * string) =
            match (cnt, snd eIdx) with
            | (cnt, e) when cnt < e.Length -> Some(e |> Seq.item cnt |> string)
            | _ -> None

        let transpose row elements =
            elements |> List.indexed |> List.map (fill row)

        let trimRight elements =
            elements
            |> List.rev
            |> List.skipWhile (fun e -> e = None)
            |> List.rev

        let padLeft elements =
            elements |> List.map (Option.defaultValue " ")

        seq {
            for row in 0 .. maxLen - 1 do
                yield! [ input
                         |> transpose row
                         |> trimRight
                         |> padLeft
                         |> List.reduce (+) ]
        }
        |> Seq.toList

    match input with
    | [] -> []
    | _ -> transpose' input
