module CarsAssemble

let rate = 221

let successRate (speed: int) : float =
    if speed >= 0 && speed < 1 then 0.0
    elif speed >= 1 && speed <= 4 then 1.0
    elif speed >= 5 && speed <= 8 then 0.9
    elif speed >= 9 && speed < 10 then 0.8
    elif speed >= 10 then 0.77
    else failwith $"Invalid speed: {speed}"

let productionRatePerHour (speed: int) : float =
    float (speed * rate) * (successRate speed)

let workingItemsPerMinute (speed: int) : int =
    int (productionRatePerHour speed / 60.0)
