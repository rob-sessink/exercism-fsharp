module Pangram

let alphabet = seq [ 'a' .. 'z' ]

let inSeq (c: char) (input: seq<char>) = input |> Seq.contains c

let onlyLowercaseAlphabet (input: string) =
    input.ToLower()
    |> Seq.filter (fun c -> inSeq c alphabet)

let isPangram (input: string): bool =
    alphabet
    |> Seq.forall (fun c -> inSeq c (onlyLowercaseAlphabet input))

// alternative implementation using uniq | sort | forall2
let isPangram2 (input: string): bool =
    onlyLowercaseAlphabet input
    |> Seq.distinct
    |> Seq.sort
    |> Seq.forall2 (fun l1 l2 -> l1 = l2) alphabet
