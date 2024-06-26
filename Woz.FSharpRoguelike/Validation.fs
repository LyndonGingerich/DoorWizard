module Validation

open Microsoft.FSharp.Core

open GameTypes
open Library
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
    | Open -> Error "Thet door is already open"
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

let private isValidLocation location level =
    if level |> hasCoordinate location then
        Ok location
    else
        Error "That location is not on the map"

let private isEmptyTile location level =
    if level |> locationBlocksMove location then
        Error "You can't move there"
    else
        Ok(getTile location level)

let private canReach target location =
    if location |> Vector.distanceFrom target <= 1.0 then
        Ok target
    else
        Error "You can't reach that"

let private isValidMoveDistance target location =
    if location |> Vector.distanceFrom target <= 1.0 then
        Ok target
    else
        Error "You can't move that far"

let private itemsAtLocation location level =
    let items = level |> itemsAt location

    if items |> Seq.isEmpty then
        Error "No items to take"
    else
        Ok items

let private isValidDirection direction actorId level =
    result {
        let! actor = level |> actorExists actorId
        let targetLocation = actor.Location + direction
        let! validTarget = level |> isValidLocation targetLocation
        return actor, validTarget
    }

let private testDoorWith test direction actorId level =
    result {
        let! actor, validTarget = level |> isValidDirection direction actorId
        let! _ = actor.Location |> canReach validTarget
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
        let! actor, validTarget = level |> isValidDirection direction actorId
        let! _ = actor.Location |> isValidMoveDistance validTarget
        let! _ = level |> isEmptyTile validTarget
        return level
    }

let canOpenDoor = testDoorWith canDoorBeOpened

let canCloseDoor = testDoorWith canDoorBeClosed

let isLockedDoor = testDoorWith canDoorBeUnlocked

let canUnlockDoor = testDoorWith hasKeyForLockedDoor

let canTakeItems direction actorId level =
    result {
        let! actor, validTarget = level |> isValidDirection direction actorId
        let! _ = actor.Location |> canReach validTarget
        let! _ = level |> itemsAtLocation validTarget
        return level
    }
