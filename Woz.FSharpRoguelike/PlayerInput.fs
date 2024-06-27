module PlayerInput

open System

open Library
open GameTypes
open Commands
open Vector.Directions
open Validation

let private selectActorCommand direction actorId level =
    let result1 =
        level
        |> isLockedDoor direction actorId
        |> OperationResult.bind (buildUnlockDoorCommand direction actorId)
        |> OperationResult.bind (buildOpenDoorCommand direction actorId)

    let result2 =
        result1.Contents
        |> Option.defaultValue level
        |> buildMoveActorCommand direction actorId

    { result2 with
        Messages = result1.Messages @ result2.Messages }

let rec handleKeyPress activeBuilder actorId level =
    let workingBuilder =
        match activeBuilder with
        | Some builder -> builder
        | None -> selectActorCommand

    let inputKey = Console.ReadKey().Key

    if level.Messages.IsEmpty then
        match inputKey with
        | ConsoleKey.D5 -> handleKeyPress (Some buildOpenDoorCommand) actorId
        | ConsoleKey.C -> handleKeyPress (Some buildCloseDoorCommand) actorId
        | ConsoleKey.U -> handleKeyPress (Some buildUnlockDoorCommand) actorId
        | ConsoleKey.T -> handleKeyPress (Some buildTakeItemsCommand) actorId
        | ConsoleKey.D9 -> workingBuilder northEast actorId
        | ConsoleKey.D8 -> workingBuilder north actorId
        | ConsoleKey.D7 -> workingBuilder northWest actorId
        | ConsoleKey.D6 -> workingBuilder east actorId
        | ConsoleKey.D4 -> workingBuilder west actorId
        | ConsoleKey.D3 -> workingBuilder southEast actorId
        | ConsoleKey.D2 -> workingBuilder south actorId
        | ConsoleKey.D1 -> workingBuilder southWest actorId
        | ConsoleKey.OemPeriod -> idleCommand
        | _ -> invalidCommand
        <| level
    else
        OperationResult.success level

let getCommandForActor = handleKeyPress None

let getPlayerCommand () = playerId |> getCommandForActor
