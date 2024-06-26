module Validation

open Microsoft.FSharp.Core

open GameTypes
open Library.Result
open Queries.Level

// Building blocks

let private actorExists actorId level =
    match level |> findActor actorId with
    | Some actor -> Ok actor
    | None -> Error "The actor does not exist"

let private doorExists location level =
    match level |> findDoor location with
    | Some door -> Ok door
    | None -> Error "There is no door there"

// actor ignored, give better shape for later
let private canDoorBeOpened actor door =
    match door with
    | Closed -> Ok door
    | Open -> Error "That door is already open"
    | Locked _ -> Error "That door is locked"

// actor ignored, give better shape for later
let private canDoorBeClosed actor door =
    match door with
    | Open -> Ok door
    | Closed -> Error "That door is already closed"
    | Locked _ -> Error "That door is locked closed"

// actor ignored, give better shape for later
let private canDoorBeUnlocked actor door =
    match door with
    | Locked keyName -> Ok keyName
    | _ -> Error "That door is not locked"

let private hasKeyForDoor keyName actor =
    if actor |> hasKey keyName then
        Ok keyName
    else
        Error("You need " + keyName + " to unlock that door")

let private isValidLocation location =
    if hasCoordinate location then
        Ok location
    else
        Error "That location is not on the map"

let private isEmptyTile location level =
    if level |> locationBlocksMove location then
        Error "You can't move there"
    else
        Ok(getTile location level)

let private isValidDirection direction actorId level =
    result {
        let! actor = level |> actorExists actorId
        let targetLocation = actor.Location + direction
        let! validTarget = isValidLocation targetLocation
        return actor, validTarget
    }

let private testDoorWith test direction actorId level =
    result {
        let! actor, validTarget = level |> isValidDirection direction actorId
        let! door = level |> doorExists validTarget
        let! _ = door |> test actor
        return level
    }

let private hasKeyForLockedDoor actor door =
    result {
        let! keyName = door |> canDoorBeUnlocked actor
        let! _ = actor |> hasKeyForDoor keyName
        return door
    }

// Validators

let isValidMove direction actorId level =
    result {
        let! _, validTarget = level |> isValidDirection direction actorId
        let! _ = level |> isEmptyTile validTarget
        return level
    }

let canOpenDoor = testDoorWith canDoorBeOpened

let canCloseDoor = testDoorWith canDoorBeClosed

let isLockedDoor = testDoorWith canDoorBeUnlocked

let canUnlockDoor = testDoorWith hasKeyForLockedDoor

let canTakeItems direction actorId level =
    result {
        let! _, validTarget = level |> isValidDirection direction actorId

        if itemsAt validTarget level |> List.isEmpty then
            return! Error "No items to take"
        else
            return level
    }
