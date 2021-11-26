module Poker

type PokerHand =
    | HighCard
    | Pair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush
    | RoyalFlush

type Suit =
    | Clubs
    | Diamonds
    | Hearts
    | Spades

type Value =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

type CardSuit =
    | CardSuit of string * Suit

    static member Create(suit: string) =
        CardSuit
        <| (suit,
            match suit with
            | "C" -> Clubs
            | "D" -> Diamonds
            | "H" -> Hearts
            | "S" -> Spades
            | _ -> invalidArg "suit" $"Unknown suit: {suit}")

    member this.Val = this |> fun (CardSuit (s, _)) -> s
    member this.Rank = this |> fun (CardSuit (_, r)) -> r

type CardValue =
    | CardValue of string * Value

    static member Create(value: string) =
        CardValue
        <| (value,
            match value with
            | "A" -> Ace
            | "K" -> King
            | "Q" -> Queen
            | "J" -> Jack
            | "10" -> Ten
            | "9" -> Nine
            | "8" -> Eight
            | "7" -> Seven
            | "6" -> Six
            | "5" -> Five
            | "4" -> Four
            | "3" -> Three
            | "2" -> Two
            | "1" -> One
            | _ -> invalidArg "value" $"Unknown value: {value}")

    member this.Val = this |> fun (CardValue (v, _)) -> v
    member this.Rank = this |> fun (CardValue (_, r)) -> r
    member this.CompareByValue(other: CardValue) = Operators.compare this.Rank other.Rank

type Card =
    { Suit: CardSuit
      Value: CardValue }

    static member Create(card: string) =
        match card with
        | c when c.Length = 2 ->
            { Value = CardValue.Create(c.[0] |> string)
              Suit = CardSuit.Create(c.[1] |> string) }
        | c when c.Length = 3 ->
            { Value = CardValue.Create(c.[..1] |> string)
              Suit = CardSuit.Create(c.[2] |> string) }
        | _ -> invalidArg "card" $"Unknown card: {card}"

    member this.ToString = this.Value.Val + this.Suit.Val

type PlayerHand =
    { Cards: Card list }

    static member Create(cards: string list) =
        match cards with
        | c when c.Length <> 5 -> invalidArg "card" $"Invalid number of cards: {cards}"
        | c -> { Cards = List.map Card.Create c }

    member private this.Create(cards: Card list) = { Cards = cards }

    /// get cards matching value
    member private this.ByValue(value: CardValue) =
        this.Cards
        |> List.filter (fun c -> c.Value = value)

    /// get cards matching suit
    member private this.BySuit(suit: CardSuit) =
        this.Cards |> List.filter (fun c -> c.Suit = suit)

    /// get highest card
    member this.HighCard =
        this.Cards |> List.maxBy (fun c -> c.Value)

    /// get CardValues for which 'x' cards have an equal value
    member this.EqualValues number =
        this.Cards
        |> List.countBy (fun c -> c.Value)
        |> List.filter (fun (_, count) -> count = number)

    /// get CardSuits for which 'x' cards have an equal suit
    member this.EqualSuits number =
        this.Cards
        |> List.countBy (fun c -> c.Suit)
        |> List.filter (fun (_, count) -> count = number)

    /// determine if cards are sequential
    member this.Sequential =
        this.Cards
        |> List.map (fun c -> c.Value.Rank)
        |> List.sort
        |> List.pairwise
        |> List.forall ((<||) compare >> (=) -1)

    // take 'y' matches (i.e pair/triplet etc), and get cards by function (value or suit)
    member private this.extractCards matches fn cards =
        List.take matches cards
        |> List.map (fun (valueOrSuit, _) -> fn valueOrSuit)
        |> List.concat
        |> Some

    /// get cards if 'x' cards have an equal value
    member this.EqualValuedCards cards matches =
        match this.EqualValues cards with
        | c when c.Length = matches -> this.extractCards matches this.ByValue c
        | _ -> None

    /// get cards if 'x' cards have an equal suit
    member this.EqualSuitedCards cards matches =
        match this.EqualSuits cards with
        | c when c.Length = matches -> this.extractCards matches this.BySuit c
        | _ -> None

    /// if the hand contains an Ace, return cards with the Ace replaced as One otherwise original cards are returned
    member this.ProjectAceAsOne =
        this.Cards
        |> List.map
            (fun (c: Card) ->
                if c.Value.Rank <> Value.Ace then
                    c
                else
                    { c with Value = CardValue.Create("1") })
        |> this.Create

    member this.ToString =
        this.Cards
        |> List.map (fun c -> c.ToString)
        |> String.concat " "

type RankedHand =
    { PokerHand: PokerHand
      PlayerHand: PlayerHand
      RankedCards: Card list }

    static member Create(pokerHand, rankedCards, playerHand) =
        { PokerHand = pokerHand
          PlayerHand = playerHand
          RankedCards = rankedCards }

    static member ToList(hands: RankedHand list) =
        hands |> List.map (fun r -> r.PlayerHand.ToString)

    /// determine remaining cards, if all cards are part of ranked card, than these are classified
    member private this.Remaining =
        match (List.except this.RankedCards this.PlayerHand.Cards) with
        | [] -> this.PlayerHand.Cards
        | c -> c

    /// sort cards top to bottom
    member private this.sortByValue(cards: Card list) =
        cards
        |> List.sortByDescending (fun c -> c.Value.Rank)

    /// compare by highest card; used as a tie breaker for equal poker hands or ranks
    member private this.CompareCardsByValue(x, y) =
        let byValue xc yc = xc.Value.CompareByValue(yc.Value)
        let l1 = this.sortByValue x
        let l2 = this.sortByValue y
        List.compareWith byValue l1 l2

    /// compare cards through comparision function
    member private this.CompareCards(x: Card list, y: Card list, fn) =
        match fn (x, y) with
        | i when i > 0 -> Some 1
        | i when i < 0 -> Some -1
        | i when i = 0 -> None
        | i -> invalidArg "compare cards" $"Unknown comparison outcome: {i}"

    /// compare hands first by ranked cards and second by remaining
    member private this.CompareByRankOrRemaining(other) =
        let cmp =
            [ (this.RankedCards, other.RankedCards)
              (this.Remaining, other.Remaining) ]
            |> List.tryPick (fun (x, y) -> this.CompareCards(x, y, this.CompareCardsByValue))

        match cmp with
        | Some 1 -> [ this ]
        | Some -1 -> [ other ]
        | None -> [ other; this ]
        | i -> invalidArg "compare rank or remaining" $"Unknown comparison outcome: {i}"

    member this.CompareWith(other) =
        match this, other with
        | x, y when x.PokerHand > y.PokerHand -> [ x ]
        | x, y when x.PokerHand < y.PokerHand -> [ y ]
        | x, y when x.PokerHand = y.PokerHand -> this.CompareByRankOrRemaining(y)
        | i -> invalidArg "compare hand" $"Unknown comparison outcome: {i}"

/// Active Patterns used in ranking a PokerHand
let (|HasValue|_|) (value: CardValue) (card: Card) =
    if value.Rank = card.Value.Rank then
        Some card
    else
        None

let (|HighCardAce|_|) (hand: PlayerHand) =
    let Ace = CardValue.Create "A"

    match hand.HighCard with
    | HasValue Ace c -> Some c
    | _ -> None

let (|HighCard|_|) (hand: PlayerHand) =
    let cards = hand.EqualValues 1

    match cards with
    | c when c.Length = 5 ->
        let cards = hand.HighCard
        Some [ cards ]
    | _ -> None

let (|Pair|_|) (hand: PlayerHand) = hand.EqualValuedCards 2 1
let (|TwoPair|_|) (hand: PlayerHand) = hand.EqualValuedCards 2 2
let (|ThreeOfAKind|_|) (hand: PlayerHand) = hand.EqualValuedCards 3 1

let rec (|Straight|_|) (hand: PlayerHand) =
    match hand with
    | h when hand.Sequential && h.Cards.Length = 5 -> Some h.Cards
    | HighCardAce _ ->
        // if hand contains an Ace, project the Ace(s) as One and recursively match for a straight
        match hand.ProjectAceAsOne with
        | Straight h -> Some h
        | _ -> None
    | _ -> None

let (|Flush|_|) (hand: PlayerHand) = hand.EqualSuitedCards 5 1
let (|FourOfAKind|_|) (hand: PlayerHand) = hand.EqualValuedCards 4 1

let (|FullHouse|_|) (hand: PlayerHand) =
    match hand with
    | ThreeOfAKind c & Pair _ -> Some c
    | _ -> None

let (|StraightFlush|_|) (hand: PlayerHand) =
    match hand with
    | Flush _ & Straight c -> Some c
    | _ -> None

let (|RoyalFlush|_|) (hand: PlayerHand) =
    match hand with
    | HighCard _ & Flush _ & Straight c -> Some c
    | _ -> None

type Round =
    { PlayerHands: PlayerHand list }

    static member Create(hands: string list) =
        let playerHands =
            hands
            |> List.map (fun hand -> hand.Split ' ')
            |> List.map (fun card -> card |> Seq.toList |> PlayerHand.Create)

        { PlayerHands = playerHands }

    member private this.RankHand(playerHand) =
        let pokerHand, rankedCards =
            match playerHand with
            | RoyalFlush cards -> RoyalFlush, cards
            | StraightFlush cards -> StraightFlush, cards
            | FourOfAKind cards -> FourOfAKind, cards
            | FullHouse cards -> FullHouse, cards
            | Flush cards -> Flush, cards
            | Straight cards -> Straight, cards
            | ThreeOfAKind cards -> ThreeOfAKind, cards
            | TwoPair cards -> TwoPair, cards
            | Pair cards -> Pair, cards
            | HighCard cards -> HighCard, cards
            | _ -> invalidArg "playerHand" $"Unknown hand: {playerHand}"

        RankedHand.Create(pokerHand, rankedCards, playerHand)

    member this.Winners =
        let rankedHands =
            this.PlayerHands |> List.map this.RankHand

        ([], rankedHands)
        ||> List.fold
                (fun s (r: RankedHand) ->
                    match s with
                    | [] -> r :: s
                    | h :: _ -> r.CompareWith(h))

let bestHands (hands: string list) =
    let round = hands |> Round.Create
    round.Winners |> RankedHand.ToList
