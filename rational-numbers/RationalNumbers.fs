module RationalNumbers

type RationalNumber = { Numerator: int; Denominator: int }

let rec gcd x y = if y = 0 then x else gcd y (x % y)

let create numerator denominator =
    { Numerator = numerator
      Denominator = denominator }

let reduce r =
    let divisor =
        gcd (abs r.Numerator) (abs r.Denominator)

    match r.Denominator with
    | x when x >= 0 -> create (r.Numerator / divisor) (r.Denominator / divisor)
    | _ -> create (r.Numerator * -1 / divisor) (r.Denominator * -1 / divisor)

let reduced numerator denominator = create numerator denominator |> reduce

let add r1 r2 =
    reduced
        (r1.Numerator
         * r2.Denominator
         + r2.Numerator * r1.Denominator)
        (r1.Denominator * r2.Denominator)

let sub r1 r2 =
    reduced
        (r1.Numerator
         * r2.Denominator
         - r2.Numerator * r1.Denominator)
        (r1.Denominator * r2.Denominator)

let mul r1 r2 =
    reduced (r1.Numerator * r2.Numerator) (r1.Denominator * r2.Denominator)

let div r1 r2 =
    reduced (r1.Numerator * r2.Denominator) (r2.Numerator * r1.Denominator)

let abs r =
    reduced (abs r.Numerator) (abs r.Denominator)

let exprational n r =
    match n with
    | e when e >= 0 -> create (pown r.Numerator e) (pown r.Denominator e)
    | e -> create (pown r.Denominator (e * -1)) (pown r.Numerator (e * -1))

let expreal r n =
    float n
    ** (float r.Numerator / float r.Denominator)
