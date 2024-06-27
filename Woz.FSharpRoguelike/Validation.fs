module Validation

open Microsoft.FSharp.Core

open GameTypes
open Library
open Queries.Level

let private doorExists location level =
    OperationResult.ofOption "There is no door there" (findDoor location level)

let private canDoorBeOpened actor door =
    match door with
    | Closed -> OperationResult.success door
    | Open -> OperationResult.failure "That door is already open"
    | Locked _ -> OperationResult.failure "That door is locked"

let private canDoorBeClosed actor door =
    match door with
    | Open -> OperationResult.success door
    | Closed -> OperationResult.failure "That door is already closed"
    | Locked _ -> OperationResult.failure "That door is locked closed"

let private canDoorBeUnlocked actor door =
    match door with
    | Locked keyName -> OperationResult.success keyName
    | _ -> OperationResult.failure "That door is not locked"

let private isValidLocation location =
    if hasCoordinate location then
        OperationResult.success location
    else
        OperationResult.failure "That location is not on the map"

let private isValidDirection direction actorId level =
    result {
        let actor = level.Actors[actorId]
        let targetLocation = actor.Location + direction
        let! validTarget = isValidLocation targetLocation
        return validTarget
    }

let private testDoorWith test direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId
        let! door = level |> doorExists validTarget
        let! _ = door |> test level.Actors[actorId]
        return level
    }

let private hasKeyForLockedDoor actor door =
    result {
        let! keyName = door |> canDoorBeUnlocked actor

        if not (hasKey keyName actor) then
            return! OperationResult.failure ("You need " + keyName + " to unlock that door")
        else
            return door
    }

// Validators

let isValidMove direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId

        if locationBlocksMove validTarget level then
            return! OperationResult.failure "You can't move there"
        else
            return level
    }

let canOpenDoor = testDoorWith canDoorBeOpened

let canCloseDoor = testDoorWith canDoorBeClosed

let isLockedDoor = testDoorWith canDoorBeUnlocked

let canUnlockDoor = testDoorWith hasKeyForLockedDoor

let canTakeItems direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId

        if itemsAt validTarget level |> List.isEmpty then
            return! OperationResult.failure "No items to take"
        else
            return level
    }
