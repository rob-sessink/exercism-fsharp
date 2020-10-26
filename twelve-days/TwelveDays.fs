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

    let rec appendGifts gifts present giftList =
        match gifts with
        | head :: tail when present = 0 -> appendGifts tail (present + 1) (sprintf "%s%s." head giftList)
        | head :: tail when present = 1 -> appendGifts tail (present + 1) (sprintf "%s, and %s" head giftList)
        | head :: tail -> appendGifts tail (present + 1) (sprintf "%s, %s" head giftList)
        | [] -> giftList

    let prefix day =
        sprintf "On the %s day of Christmas my true love gave to me: " days.[day]

    let listOfGifts day = appendGifts gifts.[0..day] 0 ""

    let verse day =
        (sprintf "%s%s" (prefix day) (listOfGifts day))

    let rec reciteVerse verses last verseList =
        let day = last - 1
        match verses with
        | _ :: tail ->
            let newVerses = (verse day) :: verseList

            reciteVerse tail day newVerses
        | [] -> verseList

    let verses = [ start .. stop ]
    reciteVerse verses stop []
