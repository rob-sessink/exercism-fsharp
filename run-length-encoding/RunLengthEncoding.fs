module RunLengthEncoding

open System

let replaceHead newHead list =
    match list with
    | _ :: tail -> newHead :: tail
    | [] -> [ newHead ]

let encode (input: string) =
    let pack entry =
        match fst entry, snd entry with
        | c, p when c = 1 -> string p
        | c, p -> string c + string p

    let encodePoint curr encoded =
        let encodeAppend curr prev encoded =
            if curr <> snd prev then (1, curr) :: encoded else replaceHead (fst prev + 1, curr) encoded

        match encoded with
        | prev :: _ -> (encodeAppend curr prev encoded)
        | [] -> [ (1, curr) ]

    let encoded =
        ([], input)
        ||> Seq.fold (fun encoded curr -> (encodePoint curr encoded))

    (encoded, "") ||> Seq.foldBack (fun curr s -> s + pack curr)

let decode input =
    let unpack entry =
        let c = fst entry |> int

        match snd entry with
        | None -> ""
        | Some p -> String.replicate c (string p)

    let decodePoint curr decoded =
        let decodeAppend curr prev decoded =
            match Char.IsDigit(curr), snd prev, fst prev with
            | true, None, _ -> (fst prev + string curr, None) :: decoded
            | true, Some _, _ -> (string curr, None) :: decoded
            | false, Some _, _ -> ("1", Some curr) :: decoded
            | false, None, "" -> ("1", Some curr) :: decoded
            | _ -> replaceHead (fst prev, Some curr) decoded

        match decoded with
        | prev :: _ -> (decodeAppend curr prev decoded)
        | [] -> (decodeAppend curr ("", None) decoded)

    let decoded =
        ([], input)
        ||> Seq.fold (fun decoded curr -> (decodePoint curr decoded))

    (decoded, "")
    ||> Seq.foldBack (fun curr s -> s + unpack curr)
