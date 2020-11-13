// Learn more about F# at http://fsharp.org

open System
open Deck
open Game



[<EntryPoint>]
let main argv =
    printfn "Crazy Eights - Functional Event Sourcing version !!"

    let store = EventStore.create()
    

    0 // return an integer exit code
