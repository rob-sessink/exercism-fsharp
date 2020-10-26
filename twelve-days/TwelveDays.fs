module TwelveDays

let recite start stop =
    let days =
        [ "first"
          "second"
          "third"
          "fourth"
          "fifth"
          "sixth"
          "seventh"
          "eighth"
          "ninth"
          "tenth"
          "eleventh"
          "twelfth" ]

    let gifts =
        [ "a Partridge in a Pear Tree"
          "two Turtle Doves"
          "three French Hens"
          "four Calling Birds"
          "five Gold Rings"
          "six Geese-a-Laying"
          "seven Swans-a-Swimming"
          "eight Maids-a-Milking"
          "nine Ladies Dancing"
          "ten Lords-a-Leaping"
          "eleven Pipers Piping"
          "twelve Drummers Drumming" ]

    let rec appendGifts gifts presents giftList =
        match gifts with
        | head :: tail when presents = 0 -> appendGifts tail (presents + 1) (sprintf "%s%s." head giftList)
        | head :: tail when presents = 1 -> appendGifts tail (presents + 1) (sprintf "%s, and %s" head giftList)
        | head :: tail -> appendGifts tail (presents + 1) (sprintf "%s, %s" head giftList)
        | [] -> giftList

    let prefix day =
        sprintf "On the %s day of Christmas my true love gave to me: " days.[day]

    let listOfGifts day = appendGifts gifts.[0..day] 0 ""

    let sentence day =
        (sprintf "%s%s" (prefix day) (listOfGifts day))

    let rec reciteVerse verses day sentences =
        let day = day - 1
        match verses with
        | _ :: tail -> reciteVerse tail day ((sentence day) :: sentences)
        | [] -> sentences

    let verses = [ start .. stop ]
    reciteVerse verses stop []
