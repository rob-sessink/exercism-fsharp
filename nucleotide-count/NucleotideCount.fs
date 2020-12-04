module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> =

    let nucleotides = seq [ 'A'; 'C'; 'G'; 'T' ]

    let zeroed =
        Seq.fold (fun (map: Map<char, int>) t -> map.Add(t, 0)) Map.empty nucleotides

    let valid strand =
        Seq.forall (fun nu -> (Seq.contains nu nucleotides)) strand

    let count strand =
        strand
        |> Seq.countBy (fun nu -> nu)
        |> Seq.fold (fun (map: Map<char, int>) t -> map.Add(fst t, snd t)) zeroed

    match valid strand with
    | true -> Some(count strand)
    | false -> None
