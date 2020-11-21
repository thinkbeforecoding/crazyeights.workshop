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

and GameStarted =
    { FirstCard: Card
      Players: Players}

// we also add the player to the event to remember who played
and Played =
    { Card: Card 
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

// for step 10, we have to remember current player
// but also the number of players around the table
// to be able to know when we made a full round
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

        [ GameStarted { Players = cmd.Players; FirstCard = cmd.FirstCard }]

    | Started _, StartGame _ ->
        raise GameAlreadyStartedException

    // for step 7, we can just return a Played event
    // for any Play command... we don't test more
    // for step 8, we make a narrower match
    | Started s, Play cmd ->

        // for step 10, we have to check the player that is playing the card
        if s.Table.CurrentPlayer <> cmd.Player then
            // oops this this not this players turn !!
            [ WrongPlayerPlayed { Card = cmd.Card; Player = cmd.Player}  ]
        elif s.TopCard.Rank = cmd.Card.Rank || s.TopCard.Suit = cmd.Card.Suit then
            // this is same rank or same suit, the card is played
            [ Played { Card = cmd.Card; Player = cmd.Player} ]
        else
            // this should be same rank or same suit!
            [ WrongCardPlayed { Card = cmd.Card; Player = cmd.Player }]

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
    | _, GameStarted e -> Started { TopCard = e.FirstCard; Table = Table.start e.Players |> Table.next }

    // here when the game is started we have access to
    // the number of players
    | Started s, Played e -> 
        Started { s with TopCard = e.Card
                         // and we compute the new current player using
                         // a modulo
                         Table = Table.next s.Table
                    }

    // we catch all other cases here
    | InitialState, Played _ 
    | _, WrongCardPlayed _ 
    | _, WrongPlayerPlayed _ -> state