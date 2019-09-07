module Deck

type Rank =
    | Ace | Two | Three | Four | Five | Six | Seven | Eight
    | Nine | Ten | Jack | Queen | King

type Suit =
    | Heart
    | Diamond
    | Club
    | Spade

type Card =
    { Rank: Rank
      Suit: Suit }

let (^) rank suit = { Rank = rank; Suit = suit }

type Player = Player of int
type Players = Players of int
