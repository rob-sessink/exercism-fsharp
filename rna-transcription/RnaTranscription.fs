module RnaTranscription

let toRna (dna: string): string =

    let toRna nu =
        match string nu with
        | "G" -> "C"
        | "C" -> "G"
        | "T" -> "A"
        | "A" -> "U"
        | _ -> failwith "Unknown nucleotide"

    dna
    |> Seq.map toRna
    |> Seq.fold (+) ""
