module Program

open ParallelLetterFrequency

[<EntryPoint>]
let main args =

    let odeAnDieFreude =
        "Freude schöner Götterfunken\n"
        + "Tochter aus Elysium,\n"
        + "Wir betreten feuertrunken,\n"
        + "Himmlische, dein Heiligtum!\n"
        + "Deine Zauber binden wieder\n"
        + "Was die Mode streng geteilt;\n"
        + "Alle Menschen werden Brüder,\n"
        + "Wo dein sanfter Flügel weilt."

    frequency [ odeAnDieFreude ] |> ignore

    0
