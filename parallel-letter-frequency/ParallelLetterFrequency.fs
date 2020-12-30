module ParallelLetterFrequency

open System

let frequency (texts: string list) =

    let byLetter (texts: string list) letter =
        let total = texts |> String.Concat

        let occurence =
            total.ToLower()
            |> Seq.filter (fun l -> l = letter)
            |> Seq.length

        match occurence with
        | 0 -> None
        | _ -> Some(letter, occurence)

    let uniqueLetters (texts: string list) =
        texts
        |> Seq.map (fun s -> s.ToLower())
        |> String.Concat
        |> Seq.filter Char.IsLetter
        |> Seq.distinct

    let taskPerLetter texts =
        texts
        |> uniqueLetters
        |> Seq.map (fun letter -> async { return byLetter texts letter })

    let asDistributionMap frequencies =
        frequencies
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Map

    texts
    |> taskPerLetter
    |> Async.Parallel
    |> Async.RunSynchronously
    |> asDistributionMap
