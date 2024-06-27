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
    actorId
    =
    let test = validator direction actorId
    let action = operation direction actorId
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
