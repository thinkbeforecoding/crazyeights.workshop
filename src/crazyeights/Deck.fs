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


// we introduce the Table type for Step 12
// it will define an algebra with (Table -> Table) functions
// like next , skip, and then reverse etc.
type Table =
    { CurrentPlayer: Player
      Players: Players }

module Table =
    let start players =
        { CurrentPlayer = Player 0 // we start a table at the dealer and use next to get the next player
          Players = players }
    // computes the next player around the table
    let next table =
        // deconstruct the table
        let { CurrentPlayer = Player currentPlayer
              Players = Players players } = table
        { table with CurrentPlayer = Player ( (currentPlayer + 1) % players) }
