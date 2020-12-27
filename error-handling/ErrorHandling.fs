module ErrorHandling

open System

let allDigits input = input |> Seq.forall Char.IsDigit

let handleErrorByThrowingException () = raise (Exception("Excepted"))

let handleErrorByReturningOption (input: string) =
    match allDigits input with
    | true -> Some(int input)
    | false -> None

let handleErrorByReturningResult (input: string) =
    match allDigits input with
    | true -> Ok(int input)
    | false -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Ok r -> switchFunction r
    | Error e -> Error e

let cleanupDisposablesWhenThrowingException resource =
    using (resource) (fun _ -> raise (Exception("Disposed")))
