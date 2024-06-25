module Commands

open GameTypes
open Library
open Operations
open Result
open Validation

let private composeCommand validation operation level =
    result {
        let! validLevel = validation level
        return operation validLevel
    }

let private buildCommand validator operation direction actorId =
    let test = validator direction actorId
    let action = operation direction actorId
    composeCommand test action

let invalidCommand = Error "Unknown command"

let idleCommand (level: level) = Ok level

let buildMoveActorCommand = buildCommand isValidMove moveActor

let buildOpenDoorCommand = buildCommand canOpenDoor openDoor

let buildCloseDoorCommand = buildCommand canCloseDoor closeDoor

let buildUnlockDoorCommand = buildCommand canUnlockDoor unlockDoor

let buildTakeItemsCommand = buildCommand canTakeItems takeItems
