module EventStore

open System
open System.Text
open EventStore.Client
open System.Linq

module Utf8 =
    let ofString input =
        Encoding.UTF8.GetBytes(input: string)
        |> ReadOnlyMemory.op_Implicit


    let toString (data: ReadOnlyMemory<byte>) =
        Encoding.UTF8.GetString(data.Span)


// this is an implementation using grpc on version 20.6.1.0
// to test locally, got to https://eventstore.com
// and download version 20.6.1.0
// then run it with the following command:
// .\EventStore.ClusterNode.exe --insecure --mem-db --enable-atom-pub-over-http
// you can see emitted events in the stream browser
let createAsync() =
    async {
    let settings =
        EventStoreClientSettings.Create("esdb://localhost:2113?Tls=false");

    let store = new EventStoreClient(settings)
    return store
    }

let create() =
    createAsync() |> Async.RunSynchronously

let readAsync (store: EventStoreClient) deserialize stream version =
    async {

        let result = 
            store.ReadStreamAsync(Direction.Forwards, stream, version, 1000L)
        let! readState = result.ReadState |> Async.AwaitTask
        if readState = ReadState.StreamNotFound then
            return StreamRevision.None , []
        else
            let! allEvents = result.ToListAsync().AsTask() |> Async.AwaitTask
            
            let lastEvent =
                if allEvents.Count > 0 then
                    allEvents.[allEvents.Count - 1].OriginalEventNumber
                else
                    version

            let events =
                [ for e in allEvents do
                    yield! deserialize (e.Event.EventType, Utf8.toString e.Event.Data) ]

            return StreamRevision.FromStreamPosition lastEvent , events
    }
let read (store: EventStoreClient) deserialize stream version =
    readAsync store deserialize stream version
    |> Async.RunSynchronously

let appendAsync (store: EventStoreClient) serialize stream (expectedVersion: StreamRevision) events =
    async {
    let eventData =
        [| for e in events do
            let eventType, data = serialize e
            yield EventData(
                    Uuid.NewUuid(),
                    eventType,
                    Utf8.ofString data,
                    Nullable(),
                    "application/json")  |] 
    let! result = 
        store.AppendToStreamAsync(stream, expectedVersion, eventData)
        |> Async.AwaitTask
    return result.NextExpectedStreamRevision }

let append (store : EventStoreClient) serialize stream expectedVersion events =
    appendAsync store serialize stream expectedVersion events
    |> Async.RunSynchronously


