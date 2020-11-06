module Program


open DiffieHellman

[<EntryPoint>]
let program args  =
    let p = 7919I
    let privateKeys = [ for _ in 0 .. 10000 -> privateKey p ]
    privateKeys |> List.iter (fun x -> printfn "%A" x)

    0
