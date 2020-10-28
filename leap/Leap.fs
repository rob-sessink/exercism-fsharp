module Leap

//  4     |  100  | 400          | leap year 
//  true  | true  | true         | true      
//  true  | true  | false        | false      
//  true  | false | false        | true       
//  false | false | false        | false

let leapYear (year: int): bool =

    let modBy divisor year = year % divisor
    let modBy4 year = modBy 4 year
    let modBy100 year = modBy 100 year
    let modBy400 year = modBy 400 year

    modBy4 year = 0
    && not (modBy100 year = 0) || modBy400 year = 0