module TwoFer

let twoFer (input: string option): string =
    let twofer =
        match input with
        | Some name -> sprintf "One for %s, one for me." name
        | None _ -> "One for you, one for me."
    twofer
