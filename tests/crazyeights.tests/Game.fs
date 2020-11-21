module Game

open System
open Xunit
open Deck
open Game
open Swensen.Unquote

// Step 3:
// Implement => to make the test run
// We use List.fold with evolve, initialized and given events
// fold start from the initialState, and for each event, call
// evolve with the state and the event to compute a new state
// The fold returns current state. We pass it to decide with
// the command to get a list of the events
let (=>) events command = 
    events
    |> List.fold evolve initialState
    |> decide command
    

let (==) = (=)

let notImplemented() = failwith "No implemented"

// Step 4:
// Change the decide function to make this test pass

// we just have to return a GameStarted event every time in decide
[<Fact>]
let ``Game should start`` () =
    test 
        <@ []
           => StartGame { FirstCard = Three ^ Club ; Players = Players 4 }
           == [GameStarted { FirstCard = Three ^ Club; Effect = Next; Players = Players 4 }] @>


// Step 5:
// Change the decide function to make this test pass

// we can do basic validation in the decide function
// notice that this exception should never actually happen
// since the game interface would prevent starting the game
// when there are less than 2 players
// we still enforce the check in case of a bug, or if a user
// with bad intention tries to trick the system
// we could also avoid validation in the decide function
// by validating the data in the Players structure's constructor
[<Fact>]
let ``Playing alone is not fun`` () =
    raises<TooFewPlayersException> 
        <@ []
            => StartGame { Players = Players 1; FirstCard = Four ^ Diamond }
        @>

// Step 6:
// What should you change to make this test pass ?

// we create a GameAlreadyStartedException
// but this time we need to remember that the game already started
// to take the decision
// we add a Started case to the State type
// now we can take a different decision in decide depending on the
// current state
// of course we have to also modify evolve to that state becomes
// Started after a GameStarted event
[<Fact>]
let ``Game should not be started twice``() =
    raises<GameAlreadyStartedException>
        <@ [ GameStarted { Players= Players 2; Effect = Next; FirstCard = Six ^ Spade } ]
            => StartGame { Players = Players 3; FirstCard = Ace ^ Heart }
        @>


// Step 7:
// Make this two tests pass... doing the simplest thing that work

// we create a Play command and a Played event, and write the test
// it says that when the game started with a 3♣ and the we play a 3♠
// an Played event should be returned

// then in the decide function, we simply return a Played event for any
// Play command... we don't test more than this
[<Fact>]
let ``Card with same value can be played``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 } ]
           => Play { Card = Three ^ Spade; Player = Player 1}
           == [ Played { Card = Three ^ Spade; Effect = Next; Player = Player 1 }] @>

// this test does the same thing with a 3♣ and a 4♣
[<Fact>]
let ``Card with same suit can be played``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 } ]
           => Play { Card = Four ^ Club; Player = Player 1}
           == [ Played { Card = Four ^ Club; Effect = Next; Player = Player 1 }] @>

// Step 8:
// Make this test pass

// here, this is again a case of something that should never happen
// so we expect an exception
// so we create a new exception
// We use the pattern matching on the state to detect the situation
[<Fact>]
let ``Card can be played only once game is started``() =
    raises<GameNotYetStarted>
        <@ []
           => Play { Card = Four ^ Club; Player = Player 1} @>

// Step 9:
// What happens here ?!

// it would be tempting to create a new exception here
// that would be right if the game interface disallowed
// playing a wrong card, for instance by limiting player
// to only play valid cards

// but this is usually not what will happen, players have
// to follow the game and play the right cards

// for instance when a player plays a wrong card (neither
// same color nor same suit) they should take a penalty of
// two cards

// this is the sign that we should introduce a new event here

// we create a WrongCardPlayed event

// you can also notice that we introduced a new event but
// didn't create a new command

// now, to take the decision, we have to remember the previous
// card ! So we need to store it in the Started case. It will
// be updated in the evolve function when the GameStarted and
// when a card has been Played.
[<Fact>]
let ``Card should be same suit or same value``() =
   test 
       <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 } ]
          => Play { Card = Four ^ Diamond; Player = Player 1 }
          == [ WrongCardPlayed { Card = Four ^ Diamond; Effect = Next; Player = Player 1 }] @>
 
    // ...

// Step 10:
// What happens here ?!

// for this one, we need to remember which player should be playing
// for the second test, we can use a modulo

// of course we need to add player id to the command. This way we
// know which players plays the card. we need to updte previous tests for this

// here again we will return an event instead of an exception
// this will happen that some players will play out of their turn
// when this happens, the players will get a penalty
[<Fact>]
let ``Player should play during her turn``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 }
             Played { Card = Three ^ Spade; Effect = Next; Player = Player 1 }  // P1 3♠
             Played { Card = Four ^ Spade; Effect = Next; Player = Player 2}    // P2 4♠
             ]
           => Play { Card = Four ^ Diamond; Player = Player 3 } // this is P3's turn
           == [ Played { Card = Four ^ Diamond; Effect = Next; Player = Player 3 }] @>

// to check what happens when 
[<Fact>]
let ``Player should play during their turn or they get an error``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 }
             Played { Card = Three ^ Spade; Effect = Next; Player = Player 1 }  // P1 3♠
             Played { Card = Four ^ Spade; Effect = Next; Player = Player 2}    // P2 4♠
             ]
           => Play { Card = Four ^ Diamond; Player = Player 1 } // this is P3's turn, but P1 plays
           // P1 gets a WrongPlayerPlayed event even if the card was correct
           == [ WrongPlayerPlayed { Card = Four ^ Diamond; Effect = Next; Player = Player 1 }] @>

[<Fact>]
let ``After a table round, the dealer turn plays``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 }
             Played { Card = Three ^ Spade; Effect = Next; Player = Player 1 }  // P1 3♠
             Played { Card = Four ^ Spade; Effect = Next; Player = Player 2}    // P2 4♠
             Played { Card = Six ^ Spade; Effect = Next; Player = Player 3}    // P3 6♠
             ]
           => Play { Card = Six ^ Diamond; Player = Player 0 } // this is P0's turn
           == [ Played { Card = Six ^ Diamond; Effect = Next; Player = Player 0 }] @>

// Step 12:
// Look at the evolve function...
// It starts to contains logic.
// Try to remove the logic from the evolve function 
// to put it back in the decide function 

// the code in the evolve function computes the next player...
// this is not a problem for now, but it will become one when
// we introduce special cards, like 7 which skips next player turn
// or jack that flips direction...

// first we can extract a part of the logic by puting it in a separate module
// we introduce a Table type with a next function that computes the next state
// of the table. It will act like a small algebra

// next table is always computed in the evolve function but we'll discuss what
// we can do in next step

// Step 13:
// Seven skips next player turn

// the test is easy to write, P2 playes a 7, it skips player's 3 turn, and
// P0 must play. As long as we change nothing, the test will fail because
// until introducing this new rule, it was player's 3 turn

// we introduce a skip function in the table module that is just (next >> next)

// the simplest way to make this test pass would be to test in the evolve function
// if the card is a seven, and call skip instead of next to compute the next player

// but it would be a mistake. first, if some games are currently played, and we
// put the new code in production, the game would suddenly compute a different
// current player! that would break the game. To rebuild the game we would have
// to use the old rule for events emited before, and the new rule for new events.
// it would get worse if at some point we decide that is not 7 that skips player turn
// but 3. We would now have 3 version of the evolve function....

// to fix this, we have to decide what happens in the decide function and indicate
// the result in the event. this way, we can just use this information from the event
// and use it in the evolve function. If we change the rule, we change the decide function
// but emitted event still contain the result of old decision. No need to change and version
// the evolve function


// for instance we could compute the next player in the decide function and put it
// in the Played event. This way, we could just use it in the evolve function without
// having to compute anything

// this is functional, but we the event would give more the result than what was intended
// the extrem would be a decide function that compute every time the next state, and the evolve
// function that just use the state in the event as the next state... it looses the "change"
// aspect of the event

// here, we can put in the event the "effect" of the card: Next or Skip, and use this
// in the evolve function to compute the player.. Next and Skip will always keep the same
// meaning, so it will be stable. The decide function determines what card have what effect
// and the evolve function applies the effect always in the same manner

// if some games are in progress, and also for all games, there will be no effect in the
// Played event, when deserializing, we can use de default value Next because it was the way
// it worked before we introduce it
[<Fact>]
let ``Seven skips next player turn``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 }
             Played { Card = Three ^ Spade; Effect = Next; Player = Player 1 }  // P1 3♠
             Played { Card = Seven ^ Spade; Effect = Skip; Player = Player 2}    // P2 7♠ it skips P3 turn
             ]
           => Play { Card = Six ^ Spade; Player = Player 0 } // this is P0's turn
           == [ Played { Card = Six ^ Spade; Effect = Next; Player = Player 0 }] @>

// we have to also check that the effect of a seven is a skip
[<Fact>]
let ``Seven is a skip``() =
    test 
        <@ [ GameStarted { FirstCard = Three ^ Club ; Effect = Next; Players = Players 4 }
             Played { Card = Three ^ Spade; Effect = Next; Player = Player 1 }  // P1 3♠
             ]
           => Play { Card = Seven ^ Spade; Player = Player 2 } // this is P0's turn
           == [ Played { Card = Seven ^ Spade; Effect = Skip; Player = Player 2 }] @>


// Step 14:
// Jack flip direction
[<Fact>]
let ``Jack flip direction``() =
    notImplemented()


// Step 15:
// Make this test pass
[<Fact>]
let ``Player can interrupt``() =
    notImplemented()

// Step 16:
// Missing an interrupt is not concidered as playing at the wrong turn.
// So what happens here ?
[<Fact>]
let ``Player get no penalty when missing an interrupt``() =
    notImplemented()


