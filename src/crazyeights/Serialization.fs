module Serialization

open Newtonsoft.Json
open System.Text

open Deck
open Game


module Json =
    let serialize o =
        JsonConvert.SerializeObject(o)
    let deserialize case ofDto (input) =
        case (ofDto (JsonConvert.DeserializeObject<_>(input)))

module Suit =
    let serialize = function
        | Heart -> "H"
        | Diamond -> "D"
        | Club -> "C"
        | Spade -> "S"

    let deserialize = function
        | "H" -> Heart
        | "D" -> Diamond
        | "C" -> Club
        | "S" -> Spade
        | _ -> failwith "Unknown suit"

module Rank =
    let  serialize = function
        | Ace -> "A"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"
        | Ten -> "10"
        | Jack -> "J"
        | Queen -> "Q"
        | King -> "K"

    let deserialize = function
        | "A"-> Ace 
        | "2"-> Two
        | "3"-> Three
        | "4"-> Four
        | "5"-> Five
        | "6"-> Six
        | "7"-> Seven
        | "8"-> Eight
        | "9"-> Nine
        | "10"-> Ten
        | "J"-> Jack
        | "Q"-> Queen
        | "K"-> King
        | _ -> failwith "Unknown rank"

module Card =
    let serialize card =
        Rank.serialize card.Rank + Suit.serialize card.Suit

    let deserialize (input: string) =
        let rank = Rank.deserialize (input.Substring(0, input.Length - 1))
        let suit = Suit.deserialize (input.Substring(input.Length - 1))
        rank ^ suit



type GameStartedDto =
    { FirstCard: string 
      Players: int }

module GameStarted =
    let serialize (cmd: GameStarted) : GameStartedDto=
        let (Players p) = cmd.Players 
        { FirstCard = Card.serialize cmd.FirstCard
          Players = p }

    let deserialize (dto: GameStartedDto) : GameStarted =
        { FirstCard = Card.deserialize dto.FirstCard 
          Players = Players dto.Players}


let serialize =
    function 
    | GameStarted e -> "GameStarted", Json.serialize (GameStarted.serialize e)


let deserialize (eventType, data) =
    match eventType with
    | "GameStarted" -> [Json.deserialize GameStarted GameStarted.deserialize data]
    | _ -> []