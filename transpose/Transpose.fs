module Transpose

let transpose (input: string list) =

    let transpose' (input: string list) =
        let maxLen =
            (input |> List.maxBy String.length |> String.length)
            - 1

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

        [ 0 .. maxLen ]
        |> List.map (fun cnt ->
            input
            |> transpose cnt
            |> trimRight
            |> padLeft
            |> List.reduce (+))

    match input with
    | [] -> []
    | _ -> transpose' input
