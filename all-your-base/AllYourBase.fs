﻿module AllYourBase

// https://www.electronics-tutorials.ws/binary/bin_2.html

let rebase digits inputBase outputBase =

    // convert digit to positional notation based upon inputBase
    // [123] = 1 * 10^2 + 2 * 10^1 + 3 * 10^0
    let fromBase digits inputBase =
        digits
        |> List.rev
        |> List.indexed
        |> List.fold (fun acc elem -> acc + (snd elem) * pown inputBase (fst elem)) 0

    // divide number by base; collecting remainders
    let rec toBase outputBase acc number =
        let division = number / outputBase
        let remainder = number % outputBase

        match (division, remainder) with
        | 0, x -> (x :: acc)
        | _, x -> toBase outputBase (x :: acc) division

    // validate input parameters
    match digits, inputBase, outputBase with
    | (d, _, _) when d |> List.exists (fun x -> x < 0) -> None // negative digit
    | (d, i, _) when d |> List.exists (fun x -> x >= i) -> None // digit equal or larger than base
    | (_, i, _) when i <= 1 -> None // input base smaller or equal to 1
    | (_, _, o) when o <= 1 -> None // output base smaller or equal to 1
    | (d, i, o) -> fromBase d i |> toBase o [] |> Some
