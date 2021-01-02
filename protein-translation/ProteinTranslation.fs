module ProteinTranslation

open System

let codonProtein =
    [ "AUG", "Methionine"
      "UUU", "Phenylalanine"
      "UUC", "Phenylalanine"
      "UUA", "Leucine"
      "UUG", "Leucine"
      "UCU", "Serine"
      "UCC", "Serine"
      "UCA", "Serine"
      "UCG", "Serine"
      "UAU", "Tyrosine"
      "UAC", "Tyrosine"
      "UGU", "Cysteine"
      "UGC", "Cysteine"
      "UGG", "Tryptophan"
      "UAA", "STOP"
      "UAG", "STOP"
      "UGA", "STOP" ]
    |> Map.ofList

let proteins rna =
    rna
    |> Seq.chunkBySize 3
    |> Seq.map (fun codon -> codonProtein.[String codon])
    |> Seq.takeWhile (fun protein -> protein <> "STOP")
    |> Seq.toList
