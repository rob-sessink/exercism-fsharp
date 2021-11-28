module GuessingGame

let reply (guess: int) : string =
    match guess with
    | g when g = 42 -> "Correct"
    | g when g = 41 || g = 43 -> "So close"
    | g when g < 42 -> "Too low"
    | g when g > 43 -> "Too high"
