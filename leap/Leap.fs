module Leap

//  4     |  100  | 400          | leap year
//  true  | true  | true         | true
//  true  | true  | false        | false
//  true  | false | false        | true
//  false | false | false        | false

let leapYear (year: int): bool =

    let modBy divisor year = year % divisor = 0
    let modBy4 year = modBy 4 year
    let modBy100 year = modBy 100 year
    let modBy400 year = modBy 400 year

    modBy4 year
    && not (modBy100 year)
    || modBy400 year
