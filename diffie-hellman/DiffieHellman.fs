module DiffieHellman

open System
open System.Numerics

let privateKey (primeP:bigint) =
    let rnd = Random()

    let number =
        rnd.NextDouble() * (float primeP - 2.0) + 2.0

    BigInteger number

let rec powI x y =
    match y with
    | y when y = 0I -> 1I
    | _ -> x * powI x (y - bigint.One)

let publicKey primeP primeG privateKey = powI primeG privateKey % primeP

let secret primeP publicKey privateKey = powI publicKey privateKey % primeP
