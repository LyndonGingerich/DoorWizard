module Commands

open GameTypes
open Library
open Operations
open Validation

let private composeCommand (validation: Level -> _) (operation: Level -> Level) level =
    result {
        let! validLevel = validation level
        return operation validLevel
    }

let private buildCommand
    (validator: Vector -> int -> Level -> _)
    (operation: Vector -> int -> Level -> Level)
    direction
    =
    let test = validator direction playerId
    let action = operation direction playerId
    composeCommand test action

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

let doorBolt direction level =
    let player = level.Actors[playerId]

    let rec inner location level =
        let newLocation = location + direction

        if Queries.Level.isBlockingTile newLocation level then
            level
        else
            level |> placeDoor Open newLocation |> inner newLocation

    inner player.Location level

let doorBoltCommand direction level =
    { Contents = doorBolt direction level |> Some
      Messages = [ "Schloop!" ] }
