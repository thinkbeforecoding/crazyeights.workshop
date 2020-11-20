module Game

open Deck

type Command =
    | StartGame of StartGame

and StartGame =
    { FirstCard: Card
      Players: Players }

type Event =
    | GameStarted of GameStarted

and GameStarted =
    { FirstCard: Card
      Players: Players}

exception TooFewPlayersException

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
    []


// Step 2:
// Make the simplest implementation for the following signature
// State -> Event list -> State

// The simplest version of evolve is to return input state
// This evolution functions don't evolve anything and state
// remains the same.
let evolve state event = state