module Darts

// inclusive cartesian on upper bound and when lower bound = 0
let (>=<) x (min, max) =
    match min with
    | 0.0 -> x >= min * min && x <= max * max
    | _ -> x > min * min && x <= max * max

let score (x: double) (y: double): int =
    let real = x * x + y * y

    match real with
    | r when r >=< (5.0, 10.0) -> 1
    | r when r >=< (1.0, 5.0) -> 5
    | r when r >=< (0.0, 1.0) -> 10
    | _ -> 0
