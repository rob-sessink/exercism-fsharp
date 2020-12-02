module Clock

type Clock = Clock of hours: int * minutes: int

let create hours minutes =

    let additionalHours =
        match minutes with
        | x when x < -60 -> x / 60 + -1 // more than a negative hour
        | x when x < 0 -> -1 // within a negative hour
        | x -> x / 60

    let hours =
        match hours + additionalHours with
        | x when x < 0 && (x % 24) = 0 -> 0 // negative on midnight
        | x when x < 0 -> x % 24 + 24 // negative not on midnight
        | x -> x % 24

    let minutesRemaining =
        match minutes with
        | x when x < 0 && (x % 60) = 0 -> 0 // negative on the hour
        | x when x < 0 -> x % 60 + 60 // negative not on the hour
        | x -> x % 60

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
