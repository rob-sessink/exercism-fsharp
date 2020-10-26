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
        [ " a Partridge in a Pear Tree."
          " two Turtle Doves, and"
          " three French Hens,"
          " four Calling Birds,"
          " five Gold Rings,"
          " six Geese-a-Laying,"
          " seven Swans-a-Swimming,"
          " eight Maids-a-Milking,"
          " nine Ladies Dancing,"
          " ten Lords-a-Leaping,"
          " eleven Pipers Piping,"
          " twelve Drummers Drumming," ]

    let rec appendGifts gifts giftList =
        match gifts with
        | head :: tail -> appendGifts tail (sprintf "%s%s" head giftList)
        | [] -> giftList

    let prefix day =
        sprintf "On the %s day of Christmas my true love gave to me:" days.[day]

    let listOfGifts day = appendGifts gifts.[0..day] ""

    let rec reciteVerse verses verseList =
        match verses with
        | _ :: tail ->
            let day = tail.Length

            let newVerses =
                (sprintf "%s%s" (prefix day) (listOfGifts day))
                :: verseList

            reciteVerse tail newVerses
        | [] -> verseList

    let verses = [ start .. stop ]
    reciteVerse verses []
