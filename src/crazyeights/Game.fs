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

exception ToFewPlayersException

// Step 1:
// Make the simplest implementation for the following signature
// Command -> State -> Event list Result
let decide command state = failwith "Not implemented"


// Step 2:
// Make the simplest implementation for the following signature
// State -> Event list -> State
let evolve state event = failwith "Not implemented" 