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
        ([], (Seq.toList input))
        ||> List.fold (fun encoded curr -> (encodePoint curr encoded))

    (encoded, "")
    ||> List.foldBack (fun curr s -> s + pack curr)


let isDigit c = Char.IsDigit(c)
let emptyPoint t = (snd t = "")
let hasPoint t = (snd t <> "")
let emptyCount t = (fst t = "")

let decode input =
    let unpack entry =
        let c = int (fst entry)
        [ for _ in 1 .. c -> (snd entry) ] |> String.Concat

    let isContinuedNumber point prev = isDigit point && emptyPoint prev

    let isStartPoint point prev = isDigit point && hasPoint prev

    let isSinglePoint point prev = isDigit point = false && hasPoint prev

    let isFirstSinglePoint point prev =
        isDigit point = false && emptyCount prev && emptyPoint prev

    let decodePoint curr decoded =
        let decodeAppend curr prev decoded =
            if isContinuedNumber curr prev then (string (fst prev) + string curr, "") :: decoded
            else if isStartPoint curr prev then (string curr, "") :: decoded
            else if isSinglePoint curr prev then ("1", string curr) :: decoded
            else if isFirstSinglePoint curr prev then ("1", string curr) :: decoded
            else replaceHead (fst prev, string curr) decoded

        match decoded with
        | prev :: _ -> (decodeAppend curr prev decoded)
        | [] -> (decodeAppend curr ("", "") decoded)

    let decoded =
        ([], (Seq.toList input))
        ||> List.fold (fun decoded curr -> (decodePoint curr decoded))

    (decoded, "")
    ||> List.foldBack (fun curr s -> s + unpack curr)
