module AtbashCipher

open System

// composed functions
let alphaToNumber (letter: char) = int letter - int 'a' + 1
let numberToAtbash (number: int) = int 'z' - number + 1 |> char
let alphaToAtbash = alphaToNumber >> numberToAtbash

let atbashToNumber (atbash: char) = int 'z' - int atbash + 1
let numberToAlpha (number: int) = int 'a' + number - 1 |> char
let atbashToAlpha = atbashToNumber >> numberToAlpha

// compacted functions
let alphaToAtbash' (al: char) = int 'z' - (int al - int 'a') |> char
let atbashToAlpha' (ab: char) = (int 'z' - int ab) + int 'a' |> char
let digitToFromAtbash digit = digit

let encodeChar ch =
    match ch with
    | c when Char.IsDigit(c) -> digitToFromAtbash c
    | c when Char.IsLetter(c) -> alphaToAtbash c
    | _ -> failwith "Invalid input: " + ch

let decodeChar ch =
    match ch with
    | c when Char.IsDigit(c) -> digitToFromAtbash c
    | c when Char.IsLetter(c) -> atbashToAlpha c
    | _ -> failwith "Invalid input: " + ch

let encode (str: string) =
    str.ToLower()
    |> Seq.filter Char.IsLetterOrDigit
    |> Seq.map encodeChar
    |> Seq.chunkBySize 5
    |> Seq.map String
    |> Seq.reduce (fun s ch -> s + " " + ch)

let decode (str: string) =
    str.ToLower()
    |> Seq.filter Char.IsLetterOrDigit
    |> Seq.map decodeChar
    |> String.Concat
