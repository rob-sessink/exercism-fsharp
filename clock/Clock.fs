module Clock

type Clock = Clock of hours: int * minutes: int

let create hours minutes =

    let additionalHours =
        match minutes with
        | x when x < -60 -> x / 60 + -1 // more than a negative hour
        | x when x < 0 -> -1 // within a negative hour
        | x -> x / 60

    let modulo divisor number =
        match number with
        | x when x < 0 && (x % divisor) = 0 -> 0 // negative exact on divisor (midnight or hour)
        | x when x < 0 -> x % divisor + divisor // negative not on divisor (midnight or hour)
        | x -> x % divisor

    let hours = modulo 24 (hours + additionalHours)
    let minutesRemaining = modulo 60 minutes

    Clock(hours, minutesRemaining)

let add minutes clock =
    let (Clock (cHours, cMinutes)) = clock
    create cHours (cMinutes + minutes)

let subtract minutes clock =
    let (Clock (cHours, cMinutes)) = clock
    create cHours (cMinutes - minutes)

let display clock =
    let (Clock (hours, minutes)) = clock
    sprintf "%02i:%02i" hours minutes
