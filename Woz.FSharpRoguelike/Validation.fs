module Validation

open Microsoft.FSharp.Core

open GameTypes
open Library
open Queries.Level

let private doorExists location level =
    OperationResult.ofOption "There is no door there" (findDoor location level)

let private canDoorBeOpened door =
    match door with
    | Closed -> None
    | Open -> "That door is already open" |> Some
    | Locked _ -> "That door is locked" |> Some

let private canDoorBeClosed door =
    match door with
    | Open -> None
    | Closed -> "That door is already closed" |> Some
    | Locked _ -> "That door is locked closed" |> Some

let private canDoorBeUnlocked door =
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

let private hasKeyForLockedDoor actor door =
    result {
        let! keyName = canDoorBeUnlocked door

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

let canOpenDoor direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId
        let! door = level |> doorExists validTarget

        match canDoorBeOpened door with
        | None -> return level
        | Some error -> return! OperationResult.failure error
    }

let canCloseDoor direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId
        let! door = level |> doorExists validTarget

        match canDoorBeClosed door with
        | None -> return level
        | Some error -> return! OperationResult.failure error
    }

let isLockedDoor direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId
        let! door = level |> doorExists validTarget
        let! _ = canDoorBeUnlocked door
        return level
    }

let canUnlockDoor direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId
        let! door = level |> doorExists validTarget
        let! _ = hasKeyForLockedDoor level.Actors[actorId] door
        return level
    }

let canTakeItems direction actorId level =
    result {
        let! validTarget = level |> isValidDirection direction actorId

        if itemsAt validTarget level |> List.isEmpty then
            return! OperationResult.failure "No items to take"
        else
            return level
    }
