module LargestSeriesProduct

open System

let inline toInt c = int c - int '0'

let inline multiply list =
    List.fold (fun acc elem -> elem * acc) 1 list

let inline onlyDigits s = s |> Seq.forall Char.IsDigit

let largestProduct (input: string) seriesLength: int option =
    match input, seriesLength with
    | i, sl when i.Length < sl -> None
    | i, sl when i = "" && sl = 0 -> Some 1
    | i, sl when i = "" && sl = 1 -> None
    | i, sl when i.Length > 0 && sl = 0 -> Some 1
    | i, _ when onlyDigits i = false -> None
    | _, sl when sl < 0 -> None
    | i, sl ->
        i
        |> Seq.toList
        |> List.map (string >> int)
        |> List.windowed sl
        |> List.map (List.reduce (*))
        |> List.max
        |> Some
