module QueenAttack

// _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ W _
// _ _ _ _ _ _ _ _    _ W _ _ _ _ B _    _ _ _ _ _ _ _ _
// _ _ _ W _ _ _ _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _
// _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _
// _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _
// _ _ _ _ _ _ B _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _
// _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _
// _ _ _ _ _ _ _ _    _ _ _ _ _ _ _ _    _ _ _ _ _ _ B _

// W(2,3) - B(5,6)    W(1,1) - B(6,1)    W(0,6) - B(7,6)

let create (position: int * int) =
    match position with
    | (x, y) when x >= 0 && x < 8 && y >= 0 && y < 8 -> true
    | _ -> false

let canAttack (queen1: int * int) (queen2: int * int) =
    let attackPosition =
        match queen1, queen2 with
        | (q1r, _), (q2r, _) when q1r = q2r -> true
        | (_, q1c), (_, q2c) when q1c = q2c -> true
        | (q1r, q1c), (q2r, q2c) when abs (q1r - q2r) = abs (q1c - q2c) -> true
        | _ -> false

    create queen1 && create queen2 && attackPosition
