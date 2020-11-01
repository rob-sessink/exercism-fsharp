module Grains

let rec squared acc i =
    match i with
    | 0
    | 1 -> acc
    | _ -> squared (acc * bigint (2)) (i - 1)

let square (n: int): Result<uint64, string> =
    match n with
    | n when (n <= 0 || n > 64) -> Error "square must be between 1 and 64"
    | n -> (squared bigint.One n) |> uint64 |> Ok

let total: Result<uint64, string> =
    seq { 1 .. 64 }
    |> Seq.sumBy (squared bigint.One)
    |> uint64
    |> Ok
