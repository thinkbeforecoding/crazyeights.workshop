module Game

open Deck

type Command =
    | StartGame of StartGame
    | Play of Play

and StartGame =
    { FirstCard: Card
      Players: Players }

// for step 10 we add the player that plays the card
and Play =
    { Card: Card
      Player: Player}

// we introduce the WrongPlayerPlayed event for step 10
type Event =
    | GameStarted of GameStarted
    | Played of Played
    | WrongCardPlayed of Played
    | WrongPlayerPlayed of Played

// when the game starts with a 7 we need to skip first player's turn
// so we also put the effect here for step 13
and GameStarted =
    { FirstCard: Card
      Effect: CardEffect
      Players: Players}

// we add the effect for step 13. The deserializer should use
// default value Next for events emitted before the new rule
and Played =
    { Card: Card 
      Effect: CardEffect
      Player: Player }


exception TooFewPlayersException

exception GameAlreadyStartedException

// this exception is introduced for step 8
exception GameNotYetStarted
// We create a type for the state
// but for now, we don't have to remember
// anything.. so or State has a single value:
// InitialState...

// for step 6, we add Started state to remember
// that the game is started
type State =
    | InitialState
    | Started of Started

and Started =
    { TopCard: Card
      Table: Table }

let initialState = InitialState

// Step 1:
// Make the simplest implementation for the following signature
// Command -> State -> Event list

// the simplest version of decide is to return an empty list
// this is a decision function that always adhere to the signature
// I finally decided to use exception instead of result type.
// There are several good reasons for this:
// * The code is simpler to write / read
// * When an error occures, it should have been prevented
//   before (validation at the border/UI to restrict cases)
//  So errors should not happen except in case of bugs.
//  Exception are perfect for bugs.
let decide command state = 
    // for step 6, we do the pattern matching on
    // both state and command

    match state, command with
    | InitialState, StartGame cmd -> 
        if cmd.Players < Players 2 then
            raise TooFewPlayersException

        [ GameStarted { Players = cmd.Players
                        Effect = CardEffect.ofCard cmd.FirstCard
                        FirstCard = cmd.FirstCard }]

    | Started _, StartGame _ ->
        raise GameAlreadyStartedException

    // for step 7, we can just return a Played event
    // for any Play command... we don't test more
    // for step 8, we make a narrower match
    | Started s, Play cmd ->

        // for step 10, we have to check the player that is playing the card
        if s.Table.CurrentPlayer <> cmd.Player then
            // oops this this not this players turn !!
            [ WrongPlayerPlayed { Card = cmd.Card
                                  Effect = CardEffect.ofCard cmd.Card
                                  Player = cmd.Player}  ]
        elif s.TopCard.Rank = cmd.Card.Rank || s.TopCard.Suit = cmd.Card.Suit then
            // this is same rank or same suit, the card is played
            // for step 13, we compute the effect of the card
            [ Played { Card = cmd.Card
                       Effect = CardEffect.ofCard cmd.Card
                       Player = cmd.Player} ]
        else
            // this should be same rank or same suit!
            [ WrongCardPlayed { Card = cmd.Card
                                Effect = CardEffect.ofCard cmd.Card
                                Player = cmd.Player }]

    // With a narrower match above, we can match on
    // combination that should not happen
    | InitialState, Play _ ->
        raise GameNotYetStarted


// Step 2:
// Make the simplest implementation for the following signature
// State -> Event list -> State

// The simplest version of evolve is to return input state
// This evolution functions don't evolve anything and state
// remains the same.
let evolve state event = 
    // this time we will modidy previous state, so we will 
    // match also on state to be able to deconstruct it
    match state,event with
    // the player next to the dealer (Player 0) plays first
    // we also store the number of Players to go back to the dealer
    // after a table round

    // when the game starts with a 7, it skips first player's turn, so
    // we use Table.applyEffect 
    | _, GameStarted e -> 
        Started { TopCard = e.FirstCard
                  Table = Table.start e.Players |> Table.applyEffect e.Effect }

    | Started s, Played e -> 
        Started { s with TopCard = e.Card
                         // we apply effect decided in the decide function
                         Table = Table.applyEffect e.Effect s.Table
                    }

    // we catch all other cases here
    | InitialState, Played _ 
    | _, WrongCardPlayed _ 
    | _, WrongPlayerPlayed _ -> state