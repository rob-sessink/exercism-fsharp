module CryptoSquare

open System

let ciphertext (input: String) =

    let dimension (l: int) =
        let c = Math.Sqrt(float l) |> Math.Ceiling
        let r = (float l) / c |> Math.Ceiling
        (int r, int c)

    let normalize (r: string) =
        r.ToLower() |> Seq.filter Char.IsLetterOrDigit

    let pad s c =
        match (c - Seq.length s) with
        | i when i > 0 -> Seq.init i (fun _ -> ' ') |> Seq.append s
        | i when i < 0 || i = 0 -> s
        | _ -> failwith $"Incorrect dimensions, chunk is too long: {s}"

    let buildSquare n =
        let dims = dimension (Seq.length n)

        n
        |> Seq.chunkBySize (snd dims)
        |> Seq.map (fun s -> pad s (snd dims))
        |> Seq.transpose
        |> Seq.map String.Concat
        |> Seq.toArray
        |> String.concat " "

    match normalize input with
    | i when Seq.length i = 0 -> ""
    | i -> buildSquare i
