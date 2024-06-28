module Commands

open GameTypes
open Library
open Operations
open Validation

let private buildCommand
    (validator: Vector -> int -> Level -> _)
    (operation: Vector -> int -> Level -> Level)
    direction
    level
    =
    result {
        let! validLevel = validator direction playerId level
        return operation direction playerId validLevel
    }

let invalidCommand _ =
    OperationResult.failure "Unknown command"

let idleCommand (level: Level) = OperationResult.success level

let movePlayer move level =
    let newLevel, messages = Operations.move move level

    { Contents = Some newLevel
      Messages = messages }

let buildOpenDoorCommand = buildCommand canOpenDoor openDoor

let buildCloseDoorCommand = buildCommand canCloseDoor closeDoor

let buildUnlockDoorCommand = buildCommand canUnlockDoor unlockDoor

let buildTakeItemsCommand = buildCommand canTakeItems takeItems

let useDoorMagic command direction level =
    let player = level.Actors[playerId]

    let rec inner location level =
        let newLocation = location + direction

        if Queries.Level.isBlockingTile newLocation level then
            level
        else
            level |> command newLocation |> inner newLocation

    inner player.Location level

let doorBlastCommand direction level =
    { Contents = useDoorMagic (placeDoor Open) direction level |> Some
      Messages = [ "Schloop!" ] }

let doorStopperCommand direction level =
    { Contents = useDoorMagic removeDoor direction level |> Some
      Messages = [ "poolhcs!" ] }

let doorBoltCommand direction level =
    { Contents = useDoorMagic (placeDoor (Locked "cat")) direction level |> Some
      Messages = [ "ching ching!" ] }

let doorBeamCommand direction level =
    { Contents = useDoorMagic (placeDoor Closed) direction level |> Some
      Messages = [ "KapLoop!" ] }
