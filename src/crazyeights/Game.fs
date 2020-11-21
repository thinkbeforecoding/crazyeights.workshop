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
    | Started

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

    | Started, StartGame _ ->
        raise GameAlreadyStartedException

    // for step 7, we can just return a Played event
    // for any Play command... we don't test more
    // for step 8, we make a narrower match
    | Started, Play cmd ->
        [ Played { Card = cmd.Card} ]

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
    // for step 6 we need to change state to started
    match event with
    | GameStarted _ -> Started