module Series

let slices (str: string) length =
    match (str, length) with
    | (s, l) when l > s.Length -> None
    | (_, l) when l <= 0 -> None
    | (s, _) when s.Length <= 0 -> None
    | (s, l) ->
        s
        |> Seq.windowed l
        |> Seq.map (fun w -> new string(w))
        |> Seq.toList
        |> Some
