module Grains

let rec squared acc i =
    match i with
    | 0
    | 1 -> acc
    | _ -> squared (acc * bigint (2)) (i - 1)

let toUInt64 n = Ok(uint64 n)

let square (n: int): Result<uint64, string> =
    match n with
    | n when (n <= 0 || n > 64) -> Error "square must be between 1 and 64"
    | n -> toUInt64 (squared bigint.One n)

let total: Result<uint64, string> =
    seq { 1 .. 64 }
    |> Seq.sumBy (squared bigint.One)
    |> toUInt64
