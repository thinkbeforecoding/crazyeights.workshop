// Learn more about F# at http://fsharp.org

open System
open Deck
open Game
open EventStore.Client

// this is an event handler for a specific game
// here, we reload every event at each call
// this is ok when steams are short and the volume of command is low
let handle store streamId command =
    // first we load existing events from the stream and get the current version number
    let version, events = EventStore.read store Serialization.deserialize streamId StreamPosition.Start

    // we compute new events has we did before
    let newEvents =
        events
        |> List.fold evolve initialState
        |> decide command

    // append new events at the end of the stream
    EventStore.append store Serialization.serialize streamId version newEvents
    |> ignore // the function returns the new version, but we don't need it here



[<EntryPoint>]
let main argv =
    printfn "Crazy Eights - Functional Event Sourcing version !!"

    let store = EventStore.create()
    
    // Game start with 3♠
    handle store "game-1" (StartGame { FirstCard = Three ^ Spade; Players = Players 4 } )
    // p1 plays 1♠
    handle store "game-1" (Play { Card = Ace ^ Spade; Player = Player 1 } )
    // p2 plays 7♠, it skips next player so it's p0's turn
    handle store "game-1" (Play { Card = Seven ^ Spade; Player = Player 2 } )
    // p0 plays J♠, it flips direction, so it's p3's turn
    handle store "game-1" (Play { Card = Jack ^ Spade; Player = Player 0 } )
    // p3 plays 6♠
    handle store "game-1" (Play { Card = Six ^ Spade; Player = Player 3 } )
    // p2 plays 6♦
    handle store "game-1" (Play { Card = Six ^ Diamond; Player = Player 2 } )
    // p0 interrupt with 6♦
    handle store "game-1" (Play { Card = Six ^ Diamond; Player = Player 0 } )
    // p1 plays 2♦
    handle store "game-1" (Play { Card = Two ^ Diamond; Player = Player 1 } )






    0 // return an integer exit code
