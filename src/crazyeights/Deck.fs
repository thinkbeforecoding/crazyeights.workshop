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

// we introduce card Effect for step 13
// we introduce flip for setp 14
type CardEffect =
    | Next
    | Skip
    | Flip

module CardEffect =
    let ofCard card =
        match card.Rank with
        | Seven -> Skip
        | Jack -> Flip  // The effect of Jack is Flip
        | _ -> Next

// we introduce direction for step 14
type Direction =
    | Clockwise
    | CounterClockwise

// for step 14, we add Direction to the Table state
type Table =
    { CurrentPlayer: Player
      Direction: Direction
      Players: Players }

module Table =
    let start players =
        { CurrentPlayer = Player 0 // we start a table at the dealer and use next to get the next player
          Direction = Clockwise // when starting the game, we play clockwise
          Players = players }
    // computes the next player around the table
    let next table =
        // deconstruct the table
        let { CurrentPlayer = Player currentPlayer
              Direction = dir
              Players = Players players } = table

        match dir with
        | Clockwise ->
            // when clockwise, play in ascending order
            { table with CurrentPlayer = Player ( (currentPlayer + 1) % players) }
        | CounterClockwise ->
            // counter clockise, play in descending order
            // we add 'players' before applying module to never get a negative number
            { table with CurrentPlayer = Player ( (currentPlayer - 1 + players) % players)}

    // skips next player turn by applying next twice for step 13
    let skip = next >> next

    // this function changes direction without moving to next player
    let reverse table =
        match table.Direction with
        | Clockwise -> { table with Direction = CounterClockwise}
        | CounterClockwise -> { table with Direction = Clockwise}

    // this function reverses direction, and moves to next player in this direction
    let flip = reverse >> next
        

    // for step 14, we add the flip effect
    let applyEffect effect table =
        match effect with
        | Next -> next table
        | Skip -> skip table
        | Flip -> flip table
