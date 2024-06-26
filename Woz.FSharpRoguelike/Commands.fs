module Commands

open GameTypes
open Library
open Operations
open Result
open Validation

let private composeCommand (validation: Level -> Result<Level, string>) (operation: Level -> Level) level =
    result {
        let! validLevel = validation level
        return operation validLevel
    }

let private buildCommand
    (validator: Vector -> int -> Level -> Result<Level, string>)
    (operation: Vector -> int -> Level -> Level)
    direction
    actorId
    =
    let test = validator direction actorId
    let action = operation direction actorId
    composeCommand test action

let invalidCommand = Error "Unknown command"

let idleCommand (level: Level) = Ok level

let buildMoveActorCommand = buildCommand isValidMove moveActor

let buildOpenDoorCommand = buildCommand canOpenDoor openDoor

let buildCloseDoorCommand = buildCommand canCloseDoor closeDoor

let buildUnlockDoorCommand = buildCommand canUnlockDoor unlockDoor

let buildTakeItemsCommand = buildCommand canTakeItems takeItems
