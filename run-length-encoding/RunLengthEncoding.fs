module RunLengthEncoding

open System

let encode (input: string) =
    let pack entry =
        match fst entry, snd entry with
        | c, p when c = 1 -> string p
        | c, p -> string c + string p

    let encodePoint curr encoded =
        let encodeAppend curr prev encoded tail =
            if curr <> snd prev then (1, curr) :: encoded else (fst prev + 1, curr) :: tail

        match encoded with
        | prev :: tail -> (encodeAppend curr prev encoded tail)
        | [] -> [ (1, curr) ]

    let encoded =
        ([], input)
        ||> Seq.fold (fun encoded curr -> (encodePoint curr encoded))

    (encoded, "") ||> Seq.foldBack (fun curr s -> s + pack curr)

let decode input =
    let unpack entry =
        match entry with
        | c, Some p -> String.replicate (int c) (string p)
        | _, None -> ""

    let decodePoint curr decoded =
        let decodeAppend curr prev decoded tail =
            match Char.IsDigit(curr), snd prev, fst prev with
            | true, None, _ -> (fst prev + string curr, None) :: decoded // continued digit
            | true, Some _, _ -> (string curr, None) :: decoded // new digit
            | false, Some _, _ -> ("1", Some curr) :: decoded // single point
            | false, None, "" -> ("1", Some curr) :: decoded // first single point
            | _ -> (fst prev, Some curr) :: tail // point after digit

        match decoded with
        | prev :: tail -> (decodeAppend curr prev decoded tail)
        | [] -> (decodeAppend curr ("", None) decoded [])

    let decoded =
        ([], input)
        ||> Seq.fold (fun decoded curr -> (decodePoint curr decoded))

    (decoded, "") ||> Seq.foldBack (fun curr s -> s + unpack curr)