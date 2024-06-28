module PlayerInput

open System

open Library

open Commands
open GameTypes
open Operations
open Validation
open Vector.Directions

let getMaybeDirection =
    function
    | ConsoleKey.D9 -> northEast |> Some
    | ConsoleKey.D8 -> north |> Some
    | ConsoleKey.D7 -> northWest |> Some
    | ConsoleKey.D6 -> east |> Some
    | ConsoleKey.D4 -> west |> Some
    | ConsoleKey.D3 -> southEast |> Some
    | ConsoleKey.D2 -> south |> Some
    | ConsoleKey.D1 -> southWest |> Some
    | _ -> None

let rec handleKeyPress activeBuilder level =
    let workingBuilder = activeBuilder |> Option.defaultValue movePlayer

    let inputKey = Console.ReadKey().Key

    if level.Messages.IsEmpty then
        match getMaybeDirection inputKey with
        | Some direction -> workingBuilder direction level
        | None ->
            match inputKey with
            | ConsoleKey.O -> handleKeyPress (Some(buildCommand canOpenDoor openDoor))
            | ConsoleKey.C -> handleKeyPress (Some(buildCommand canCloseDoor closeDoor))
            | ConsoleKey.U -> handleKeyPress (Some(buildCommand canUnlockDoor unlockDoor))
            | ConsoleKey.T -> handleKeyPress (Some(buildCommand canTakeItems takeItems))
            | ConsoleKey.OemMinus -> handleKeyPress (Some doorBlastCommand)
            | ConsoleKey.OemPeriod -> handleKeyPress (Some doorStopperCommand)
            | ConsoleKey.OemPlus -> handleKeyPress (Some doorBoltCommand)
            | ConsoleKey.D5 -> handleKeyPress (Some doorBeamCommand)
            | _ -> invalidCommand
            <| level
    else
        OperationResult.success level

let getCommandForActor = handleKeyPress None

let getPlayerCommand () = getCommandForActor
