module Acronym

open System

let abbreviate (phrase: string) =
    phrase.Split([| ' '; '-'; '_' |])
    |> Seq.filter (fun w -> w.Length > 0)
    |> Seq.map (Seq.take 1)
    |> Seq.concat
    |> Seq.map Char.ToUpper
    |> String.Concat
