module Grep

open System.IO
open System.Text.RegularExpressions

type File = File of file: string * lines: string list

type MatchedLine = MatchedLine of lineNumber: int * line: string * fileName: string

let hasFlag flag flagArguments = Seq.contains flag flagArguments

let caseInsensitive flags pattern =
    if hasFlag "-i" flags then "(?i)" + pattern else pattern

let entireLine flags pattern =
    if hasFlag "-x" flags then "^" + pattern + "$" else pattern

let matches flags pattern lineIdx =
    let regex =
        caseInsensitive flags pattern |> entireLine flags

    let inverted = hasFlag "-v" flags
    Regex.Match((snd lineIdx), regex).Success
    <> inverted

let formatMultiple flags files =
    List.length files > 1 && not (hasFlag "-l" flags)

let format flags files matchedLine =
    let (MatchedLine (lineNumber, line, fileName)) = matchedLine

    let formatted =
        match flags with
        | f when hasFlag "-l" f -> sprintf "%s" fileName
        | f when hasFlag "-n" f -> sprintf "%i:%s" (lineNumber + 1) line
        | _ -> sprintf "%s" line

    match formatMultiple flags files with
    | false -> formatted
    | true -> sprintf "%s:%s" fileName formatted

let openFile file =
    (file, File.ReadAllLines(file) |> List.ofArray)

let grep' flags pattern file =
    snd file
    |> List.indexed
    |> List.filter (matches flags pattern)
    |> List.map (fun lineIdx -> MatchedLine(fst lineIdx, snd lineIdx, fst file))

let grep files flagArguments pattern =
    files
    |> List.map openFile
    |> List.map (grep' flagArguments pattern)
    |> List.concat
    |> List.map (format flagArguments files)
    |> List.distinct
