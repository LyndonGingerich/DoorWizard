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

let getMaybeNextAction =
    function
    | ConsoleKey.O -> buildCommand canOpenDoor openDoor |> Some
    | ConsoleKey.C -> buildCommand canCloseDoor closeDoor |> Some
    | ConsoleKey.U -> buildCommand canUnlockDoor unlockDoor |> Some
    | ConsoleKey.T -> buildCommand canTakeItems takeItems |> Some
    | ConsoleKey.OemMinus -> doorBlastCommand |> Some
    | ConsoleKey.OemPeriod -> doorStopperCommand |> Some
    | ConsoleKey.OemPlus -> doorBoltCommand |> Some
    | ConsoleKey.D5 -> doorBeamCommand |> Some
    | _ -> None

let handleKeyPress level =
    let workingBuilder = level.NextAction |> Option.defaultValue movePlayer

    let inputKey = Console.ReadKey().Key

    if level.Messages.IsEmpty then
        match getMaybeDirection inputKey with
        | Some direction ->
            workingBuilder direction level
            |> OperationResult.mapContents (fun level -> { level with NextAction = None })
        | None ->
            match getMaybeNextAction inputKey with
            | Some action -> OperationResult.ofTuple ({ level with NextAction = Some action }, [])
            | None ->
                match inputKey with
                | _ -> fun _ -> OperationResult.failure "Unknown command"
                <| level
    else
        OperationResult.success level
