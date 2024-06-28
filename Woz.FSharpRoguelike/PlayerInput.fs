module PlayerInput

open System

open Library

open Commands
open GameTypes
open Operations
open Validation
open Vector.Directions

let rec handleKeyPress activeBuilder level =
    let workingBuilder = activeBuilder |> Option.defaultValue movePlayer

    let inputKey = Console.ReadKey().Key

    if level.Messages.IsEmpty then
        match inputKey with
        | ConsoleKey.O -> handleKeyPress (Some(buildCommand canOpenDoor openDoor))
        | ConsoleKey.C -> handleKeyPress (Some(buildCommand canCloseDoor closeDoor))
        | ConsoleKey.U -> handleKeyPress (Some(buildCommand canUnlockDoor unlockDoor))
        | ConsoleKey.T -> handleKeyPress (Some(buildCommand canTakeItems takeItems))
        | ConsoleKey.OemMinus -> handleKeyPress (Some doorBlastCommand)
        | ConsoleKey.OemPeriod -> handleKeyPress (Some doorStopperCommand)
        | ConsoleKey.OemPlus -> handleKeyPress (Some doorBoltCommand)
        | ConsoleKey.D5 -> handleKeyPress (Some doorBeamCommand)
        | ConsoleKey.D9 -> workingBuilder northEast
        | ConsoleKey.D8 -> workingBuilder north
        | ConsoleKey.D7 -> workingBuilder northWest
        | ConsoleKey.D6 -> workingBuilder east
        | ConsoleKey.D4 -> workingBuilder west
        | ConsoleKey.D3 -> workingBuilder southEast
        | ConsoleKey.D2 -> workingBuilder south
        | ConsoleKey.D1 -> workingBuilder southWest
        | _ -> invalidCommand
        <| level
    else
        OperationResult.success level

let getCommandForActor = handleKeyPress None

let getPlayerCommand () = getCommandForActor
