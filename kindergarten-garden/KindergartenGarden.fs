module KindergartenGarden

type Plant =
    | Clover
    | Grass
    | Radishes
    | Violets

    static member Create(p) =
        match p with
        | 'C' -> Clover
        | 'G' -> Grass
        | 'R' -> Radishes
        | 'V' -> Violets
        | _ -> failwith $"Unknown plant: {p}"

let students =
    [ "Alice"
      "Bob"
      "Charlie"
      "David"
      "Eve"
      "Fred"
      "Ginny"
      "Harriet"
      "Ileana"
      "Joseph"
      "Kincaid"
      "Larry" ]

let plants (diagram: string) student =

    let ordinal (student: string) =
        List.findIndex (fun x -> x = student) students

    let getPair ord (window: string) = window.Substring(ord * 2, 2)

    let toPlants pair = pair |> Seq.map Plant.Create

    diagram.Split "\n"
    |> Seq.map (getPair (ordinal student))
    |> Seq.collect toPlants
    |> Seq.toList
