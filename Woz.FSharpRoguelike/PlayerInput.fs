module PlayerInput

open System

open Library
open GameTypes
open Commands
open Vector.Directions

let rec handleKeyPress activeBuilder level =
    let workingBuilder =
        match activeBuilder with
        | Some builder -> builder
        | None -> movePlayer

    let inputKey = Console.ReadKey().Key

    if level.Messages.IsEmpty then
        match inputKey with
        | ConsoleKey.O -> handleKeyPress (Some buildOpenDoorCommand)
        | ConsoleKey.C -> handleKeyPress (Some buildCloseDoorCommand)
        | ConsoleKey.U -> handleKeyPress (Some buildUnlockDoorCommand)
        | ConsoleKey.T -> handleKeyPress (Some buildTakeItemsCommand)
        | ConsoleKey.D5 -> handleKeyPress (Some doorBoltCommand)
        | ConsoleKey.D9 -> workingBuilder northEast
        | ConsoleKey.D8 -> workingBuilder north
        | ConsoleKey.D7 -> workingBuilder northWest
        | ConsoleKey.D6 -> workingBuilder east
        | ConsoleKey.D4 -> workingBuilder west
        | ConsoleKey.D3 -> workingBuilder southEast
        | ConsoleKey.D2 -> workingBuilder south
        | ConsoleKey.D1 -> workingBuilder southWest
        | ConsoleKey.OemPeriod -> idleCommand
        | _ -> invalidCommand
        <| level
    else
        OperationResult.success level

let getCommandForActor = handleKeyPress None

let getPlayerCommand () = getCommandForActor
