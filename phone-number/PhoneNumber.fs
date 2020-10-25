module PhoneNumber

open System
open System.Text.RegularExpressions

let clean (input: string): Result<uint64, string> =
    let (>>=) x fn = Result.bind fn x

    let validateForLettersAndPunctuations (input: string) =
        match input with
        | c when (Regex.IsMatch(c, "[a-zA-Z].")) -> Error "letters not permitted"
        | c when (Regex.IsMatch(c.[0..], "@:!?")) -> Error "punctuations not permitted"
        | c -> Ok c

    let validateLength (input: string) =
        let filtered =
            input
            |> String.filter (fun c -> Seq.contains c (seq [ '0' .. '9' ]))

        match filtered with
        | c when (c.Length < 10) -> Error "incorrect number of digits"
        | c when (c.Length > 11) -> Error "more than 11 digits"
        | c when (c.Length = 11 && c.[0] <> '1') -> Error "11 digits must start with 1"
        | c when (c.Length = 11 && c.[0] = '1') -> Ok c.[1..]
        | c -> Ok c

    let validatedAreaCode (input: string) =
        match input with
        | c when (c.[0] = '0') -> Error "area code cannot start with zero"
        | c when (c.[0] = '1') -> Error "area code cannot start with one"
        | c -> Ok c

    let validatedExchangeCode (input: string) =
        match input with
        | c when (c.[3] = '0') -> Error "exchange code cannot start with zero"
        | c when (c.[3] = '1') -> Error "exchange code cannot start with one"
        | c -> Ok c

    let toNumber (input: string) = Ok(UInt64.Parse input)

    let cleaned =
        input
        |> validateForLettersAndPunctuations
        >>= validateLength
        >>= validatedAreaCode
        >>= validatedExchangeCode
        >>= toNumber

    cleaned
