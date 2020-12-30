module ParallelLetterFrequency

open System

// Solution by counting letter frequencies for the combined texts in a task per unique letter.
// For the texts used in the unit test this solution seems a bit slower, however expectation is when
// E(unique letters) < E(number of texts) this is faster
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

    texts
    |> taskPerLetter
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Map

// Variant solution by counting letter frequencies in a task per individual text. Letter frequencies for every text
// are summed afterwards
let frequency' (texts: string list) =

    let byText (text: string) =
        text.ToLower()
        |> Seq.filter Char.IsLetter
        |> Seq.countBy id

    let taskPerText texts =
        texts
        |> Seq.map (fun text -> async { return byText text })

    let asDistributionMap frequencies =
        frequencies
        |> Seq.concat
        |> Seq.groupBy (fun freq -> fst freq)
        |> Seq.map (fun (letter, frequencies) -> (letter, frequencies |> Seq.sumBy snd))
        |> Map

    texts
    |> taskPerText
    |> Async.Parallel
    |> Async.RunSynchronously
    |> asDistributionMap
