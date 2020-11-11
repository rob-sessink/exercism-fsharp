module NthPrime

open System.Collections

let prime (nth: int): int option =

    let lni n = n |> float |> log

    // calculate upper bound for a Nth prime, based upon
    // https://stackoverflow.com/questions/33519963/finding-nth-prime-using-seive-of-eratosthens
    let upperbound nth =
        match nth with
        | 1 -> 2
        | 2 -> 5
        | 3 -> 6
        | 4 -> 7
        | 5 -> 11
        | _ -> (float nth) * (lni nth + log (lni nth)) |> int // n (log n + log log n)

    let sieveUntil until =

        let max = 1 + (until |> float |> sqrt |> int)
        let sieve = BitArray(until + 1, true)

        for i in 2 .. max do
            if sieve.[i] = true then
                for d in i * i .. i .. until do
                    sieve.[d] <- false

        let primes =
            seq {
                for i in 2 .. until do
                    if sieve.[i] = true then i
            }

        primes

    match nth with
    | 0 -> None
    | n ->
        sieveUntil (upperbound n)
        |> Seq.take nth
        |> Seq.last
        |> Some
