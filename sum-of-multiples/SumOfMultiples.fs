module SumOfMultiples

// function with smarter for / yield sequence and using collect
let sum (numbers: int list) (upperBound: int) : int =
    let multiplesOf upperBound divisor =
        seq {
            for multiple in divisor .. divisor .. (upperBound - 1) do
                yield multiple
        }

    numbers
    |> Seq.filter (fun n -> n > 0)
    |> Seq.collect (multiplesOf upperBound)
    |> Seq.distinct
    |> Seq.sum

// function using seq, yield and Option module to derive multiplesOf
let sum2 (numbers: int list) (upperBound: int) : int =
    let isMultipleOf dividend divisor =
        if divisor <> 0 && dividend % divisor = 0 then Some dividend else None

    let multiplesOf upperBound divisor =
        seq {
            for dividend in (upperBound - 1) .. -1 .. divisor do
                if Option.isSome (isMultipleOf dividend divisor) then yield dividend
        }

    numbers
    |> Seq.collect (multiplesOf upperBound)
    |> Seq.distinct
    |> Seq.sum

// function using purely Seq module members: choose, unfold to derive multiplesOf
let sum3 (numbers: int list) (upperBound: int) : int =
    let isMultipleOf dividend divisor =
        if divisor <> 0 && dividend % divisor = 0 then Some dividend else None

    let dividendsFor upperBound divisor =
        upperBound - 1
        |> Seq.unfold (fun s -> if (s >= divisor) then Some(s, s - 1) else None)

    let multiplesOf upperBound divisor =
        dividendsFor upperBound divisor
        |> Seq.choose (fun dividend -> isMultipleOf dividend divisor)

    numbers
    |> Seq.collect (multiplesOf upperBound)
    |> Seq.distinct
    |> Seq.sum
