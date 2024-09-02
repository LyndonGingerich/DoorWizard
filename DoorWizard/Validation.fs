module Validation

open Microsoft.FSharp.Core

open GameTypes
open Library
open Queries

let private doorExists location level =
    findDoor location level |> Result.ofOption "There is no door there"

let private canDoorBeClosed door =
    match door with
    | Open -> None
    | Closed -> "That door is already closed" |> Some
    | Locked _ -> "That door is locked closed" |> Some

let private canDoorBeUnlocked door =
    match door with
    | Locked keyName -> Ok keyName
    | _ -> Error "That door is not locked"

let private isValidLocation location =
    if hasCoordinate location then
        None
    else
        Some "That location is not on the map"

let private isValidDirection direction actorId level =
    let actor = level.Actors[actorId]
    let targetLocation = actor.Location + direction

    match isValidLocation targetLocation with
    | Some error -> Error error
    | None -> Ok targetLocation

let private hasKeyForLockedDoor actor door =
    match canDoorBeUnlocked door with
    | Ok keyName ->
        if hasKey keyName actor then
            None
        else
            Some("You need " + keyName + " to unlock that door")
    | Error message -> Some message

// Validators

let getDoorResult direction actorId level =
    isValidDirection direction actorId level
    |> Result.bind (fun validTarget -> doorExists validTarget level)

let canOpenDoor direction actorId level =
    match getDoorResult direction actorId level with
    | Ok door ->
        match door with
        | Closed -> None
        | Open -> Some "That door is already open"
        | Locked _ -> Some "That door is locked"
    | Error msg -> Some msg

let canCloseDoor direction actorId level =
    match getDoorResult direction actorId level with
    | Ok door -> canDoorBeClosed door
    | Error msg -> Some msg

let isLockedDoor direction actorId level =
    getDoorResult direction actorId level |> Result.bind canDoorBeUnlocked

let canUnlockDoor direction actorId level =
    match getDoorResult direction actorId level with
    | Ok door -> hasKeyForLockedDoor level.Actors[actorId] door
    | Error msg -> Some msg

let canTakeItems direction actorId level =
    match isValidDirection direction actorId level with
    | Ok validTarget ->
        if itemsAt validTarget level |> List.isEmpty then
            Some "No items to take"
        else
            None
    | Error msg -> Some msg
