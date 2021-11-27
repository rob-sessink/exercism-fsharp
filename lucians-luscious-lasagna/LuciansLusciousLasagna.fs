module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven r = expectedMinutesInOven - r

let preparationTimeInMinutes l = l * 2

let elapsedTimeInMinutes l e = preparationTimeInMinutes l + e
