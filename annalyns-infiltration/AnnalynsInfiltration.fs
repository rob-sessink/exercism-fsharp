module AnnalynsInfiltration

let canFastAttack (knightIsAwake: bool) : bool = knightIsAwake = false

let canSpy (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) : bool =
    knightIsAwake || archerIsAwake || prisonerIsAwake

let canSignalPrisoner (archerIsAwake: bool) (prisonerIsAwake: bool) : bool =
    archerIsAwake = false && prisonerIsAwake

let canFreePrisoner (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) (petDogIsPresent: bool) : bool =
    match petDogIsPresent with
    | false -> prisonerIsAwake && knightIsAwake = false && archerIsAwake = false
    | true -> archerIsAwake = false
