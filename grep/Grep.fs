module Grep

open System.IO
open System.Text.RegularExpressions

type File = File of file: string * lines: string list

type MatchedLine = MatchedLine of lineNumber: int * line: string * fileName: string

let caseInsensitive (flags: Set<string>) pattern =
    if flags.Contains("-i") then "(?i)" + pattern else pattern

let entireLine (flags: Set<string>) pattern =
    if flags.Contains("-x") then "^" + pattern + "$" else pattern

let matches (flags: Set<string>) pattern lineIdx =
    let regex = caseInsensitive flags pattern |> entireLine flags

    let inverted = flags.Contains("-v")
    Regex.Match((snd lineIdx), regex).Success <> inverted

let formatMultiple (flags: Set<string>) files = List.length files > 1 && not (flags.Contains("-l"))

let format (flags: Set<string>) files matchedLine =
    let (MatchedLine(lineNumber, line, fileName)) = matchedLine

    let formatted =
        match flags with
        | f when flags.Contains("-l") -> sprintf "%s" fileName
        | f when flags.Contains("-n") -> sprintf "%i:%s" (lineNumber + 1) line
        | _ -> sprintf "%s" line

    match formatMultiple flags files with
    | false -> formatted
    | true -> sprintf "%s:%s" fileName formatted

let openFile file = (file, File.ReadAllLines(file) |> List.ofArray)

let grep' (flags: Set<string>) pattern file =
    snd file
    |> List.indexed
    |> List.filter (matches flags pattern)
    |> List.map (fun lineIdx -> MatchedLine(fst lineIdx, snd lineIdx, fst file))

let grep files flagArguments pattern =
    let flags = flagArguments |> set

    files
    |> List.map openFile
    |> List.map (grep' flags pattern)
    |> List.concat
    |> List.map (format flags files)
    |> List.distinct
