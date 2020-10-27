module Leap

let leapYear (year: int): bool =
    match year % 4 with
    | 0 ->
        match year % 100 with
        | 0 ->
            match year % 400 with
            | 0 -> true
            | _ -> false
        | _ -> true
    | _ -> false
