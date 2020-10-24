module Isogram

open System

let isIsogram (input: string) =
    match input with
    | "" -> true
    | i ->
        i.ToLower()
        |> Seq.filter Char.IsLetter
        |> Seq.countBy (fun c -> c)
        |> Seq.maxBy snd
        |> snd = 1
