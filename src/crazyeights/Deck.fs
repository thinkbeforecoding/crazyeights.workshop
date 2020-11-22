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
// we introduce flip for step 14
// we introduce interrupt for step 15 
type CardEffect =
    | Next
    | Skip
    | Flip
    | Interrupt

// notice that interrupt is not tied to a specific card
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
    // in setp 15, interrupt does change current table
    let applyEffect effect table =
        match effect with
        | Next -> next table
        | Skip -> skip table
        | Flip -> flip table
        | Interrupt -> table

// for step 16 we introduce a Pile type
// it always has a TopCard, and each time we
// put a new card on it, the TopCard becomes NextCard
// and the new card becomes TopCard
type Pile =
    { TopCard: Card
      NextCard: Card option }

module Pile =
    // this starts the Pile with a single card
    let start card =
        { TopCard = card
          NextCard = None}

    // put a new card on top of the pile
    let put card pile =
        { TopCard = card
          NextCard = Some pile.TopCard }
