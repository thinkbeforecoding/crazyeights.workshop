module EventStore

open System
open EventStore.ClientAPI
open System.Text
open EventStore.ClientAPI.SystemData
module Utf8 =
    let ofString input =
        Encoding.UTF8.GetBytes(input: string)


    let toString data =
        Encoding.UTF8.GetString(data: byte[])

let createAsync() =
    async {
    let settings =
        ConnectionSettings.Create()
            .SetDefaultUserCredentials(UserCredentials("admin", "changeit"))
            .Build()
    let store = EventStoreConnection.Create(settings, Uri "tcp://localhost:1113")
    do! store.ConnectAsync() |> Async.AwaitTask
    return store
    }

let create() =
    createAsync() |> Async.RunSynchronously

let readAsync (store: IEventStoreConnection) deserialize stream version =
    async {

        let! slice = 
            store.ReadStreamEventsForwardAsync(stream, version, 1000, true)
            |> Async.AwaitTask

        let events =
            [ for e in slice.Events do
                yield! deserialize (e.Event.EventType, Utf8.toString e.Event.Data) ]

        return slice.LastEventNumber, events
    }
let read (store: IEventStoreConnection) deserialize stream version =
    readAsync store deserialize stream version
    |> Async.RunSynchronously

let appendAsync (store: IEventStoreConnection) serialize stream expectedVersion events =
    async {
    let eventData =
        [| for e in events do
            let eventType, data = serialize e
            yield EventData(
                    Guid.NewGuid(),
                    eventType,
                    true,
                    Utf8.ofString data,
                    null)  |] 
    let! result = 
        store.AppendToStreamAsync(stream, expectedVersion, eventData)
        |> Async.AwaitTask
    return result.NextExpectedVersion }

let append (store : IEventStoreConnection) serialize stream expectedVersion events =
    appendAsync store serialize stream expectedVersion events
    |> Async.RunSynchronously


