module TwoFer

let twoFer (input: string option): string =
    let twofer =
        sprintf "One for %s, one for me." (Option.defaultValue "you" input)

    twofer
