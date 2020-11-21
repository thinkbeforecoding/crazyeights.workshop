module Game

open Deck

type Command =
    | StartGame of StartGame
    | Play of Play

and StartGame =
    { FirstCard: Card
      Players: Players }

and Play =
    { Card: Card }

type Event =
    | GameStarted of GameStarted
    | Played of Played
    | WrongCardPlayed of Played

and GameStarted =
    { FirstCard: Card
      Players: Players}

and Played =
    { Card: Card}


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
// for step 9, we need to remember the card on the
// top of the game, there is no card ini the InitialState
// but there is always a TopCard once started
and Started =
    { TopCard: Card}

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
        // now that the Started case contains more information
        // as the top card we can use it to compare the TopCard
        // to the card beeing played
        if s.TopCard.Rank = cmd.Card.Rank || s.TopCard.Suit = cmd.Card.Suit then
            // this is same rank or same suit, the card is played
            [ Played { Card = cmd.Card} ]
        else
            // this should be same rank or same suit!
            [ WrongCardPlayed { Card = cmd.Card }]

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
    // you can can notice that there was a warning here
    // we introduced a Played event before and never match it
    // here. we could have added a _ -> state catch clause to
    // do nothing. But this is a good idea to keep a complete
    // and precise pattern matching, and pass this warning as
    // an error

    match event with
    // for step 9, we have to put in the state
    // the first card as the top card
    | GameStarted e -> Started { TopCard = e.FirstCard }
    // we also have to remember when a card is played
    | Played e -> Started { TopCard = e.Card }
    // when WrongCardPlayed this doesn't change the state of the game
    | WrongCardPlayed _ -> state