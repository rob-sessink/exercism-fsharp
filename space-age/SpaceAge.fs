module SpaceAge

type Planet = { orbitalPeriod: float }

let Mercury = { orbitalPeriod = 0.2408467 }
let Venus = { orbitalPeriod = 0.61519726 }
let Earth = { orbitalPeriod = 1.0 }
let Mars = { orbitalPeriod = 1.8808158 }
let Jupiter = { orbitalPeriod = 11.862615 }
let Saturn = { orbitalPeriod = 29.447498 }
let Uranus = { orbitalPeriod = 84.016846 }
let Neptune = { orbitalPeriod = 164.79132 }
let secondsPerEarthYear = 31557600.0

let age (planet: Planet) (seconds: int64): float =
    float seconds
    / secondsPerEarthYear
    / planet.orbitalPeriod
