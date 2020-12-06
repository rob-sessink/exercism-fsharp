module SpaceAge

type Planet =
    { orbitalPeriod: float }
    member this.secondsPerEarthYear = 31557600.0

    member this.ageInEarthYears(seconds: int64) =
        float seconds
        / this.secondsPerEarthYear
        / this.orbitalPeriod

let Mercury = { orbitalPeriod = 0.2408467 }
let Venus = { orbitalPeriod = 0.61519726 }
let Earth = { orbitalPeriod = 1.0 }
let Mars = { orbitalPeriod = 1.8808158 }
let Jupiter = { orbitalPeriod = 11.862615 }
let Saturn = { orbitalPeriod = 29.447498 }
let Uranus = { orbitalPeriod = 84.016846 }
let Neptune = { orbitalPeriod = 164.79132 }

let age (planet: Planet) (seconds: int64): float = planet.ageInEarthYears seconds
