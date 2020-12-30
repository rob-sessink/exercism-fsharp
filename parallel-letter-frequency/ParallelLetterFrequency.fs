module ParallelLetterFrequency

open System

let frequency (texts: string list) =

    let byLetter (texts: string list) letter =
        let combined = texts |> String.Concat

        let occurence =
            combined.ToLower()
            |> Seq.filter (fun l -> l = letter)
            |> Seq.length

        (letter, occurence)

    let uniqueLetters (texts: string list) =
        let combined = texts |> String.Concat

        combined.ToLower()
        |> Seq.filter Char.IsLetter
        |> Seq.distinct

    let taskPerLetter texts =
        texts
        |> uniqueLetters
        |> Seq.map (fun letter -> async { return byLetter texts letter })

    let asDistributionMap frequencies =
        frequencies
        |> Seq.filter (fun freq -> snd freq <> 0)
        |> Map

    texts
    |> taskPerLetter
    |> Async.Parallel
    |> Async.RunSynchronously
    |> asDistributionMap
