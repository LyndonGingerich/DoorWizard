module DoorWizard.Engine.PlayerInput

open System

open Commands
open GameTypes
open Operations
open Validation
open Direction

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

let getMaybeNextAction =
    function
    | ConsoleKey.O -> buildCommand canOpenDoor openDoor |> Some
    | ConsoleKey.C -> buildCommand canCloseDoor closeDoor |> Some
    | ConsoleKey.U -> buildCommand canUnlockDoor unlockDoor |> Some
    | ConsoleKey.T -> buildCommand canTakeItems takeItems |> Some
    | ConsoleKey.OemMinus -> doorBeamCommand |> Some
    | ConsoleKey.OemPeriod -> doorStopperCommand |> Some
    | ConsoleKey.OemPlus -> doorBoltCommand |> Some
    | ConsoleKey.D5 -> doorBlastCommand |> Some
    | _ -> None

let handleKeyPress inputKey level =
    let workingBuilder = level.NextAction |> Option.defaultValue move

    if level.Messages.IsEmpty then
        match getMaybeDirection inputKey with
        | Some direction ->
            let newLevel, messages = workingBuilder direction level
            { newLevel with NextAction = None }, messages
        | None ->
            match getMaybeNextAction inputKey with
            | Some action -> { level with NextAction = Some action }, []
            | None ->
                match inputKey with
                | _ -> level, [ "Unknown command" ]
    else
        level, []

let runTurn playerCommand level =
    let newLevel, messages = level |> playerCommand
    newLevel |> logAll messages

let deathMessage = "You dead, fluff butt!"
