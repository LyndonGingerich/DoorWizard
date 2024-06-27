module PlayerInput

open System

open Library
open GameTypes
open Commands
open Vector.Directions

let rec handleKeyPress activeBuilder actorId level =
    let workingBuilder =
        match activeBuilder with
        | Some builder -> builder
        | None -> buildMoveActorCommand

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
