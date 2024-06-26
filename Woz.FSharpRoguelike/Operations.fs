﻿module Operations

open GameTypes
open Library
open Queries.Level

// Messages

let log message level =
    { level with
        Messages = message :: level.Messages }

let logAll newMessages level =
    { level with
        Messages = newMessages @ level.Messages }

let flush level = { level with Messages = [] }

// Actor

let actorTarget direction actorId level =
    let actor = level |> getActor actorId
    let targetLocation = actor.Location + direction
    actor, targetLocation

let spawnActor actor level =
    { level with
        Actors = Map.add actor.Id actor level.Actors
        MapActors = Map.add actor.Location actor.Id level.MapActors }

let removeActor actorId level =
    if actorId = playerId then
        failwith "Must not remove the player without ending the game"

    let actor = level |> getActor actorId

    { level with
        Actors = Map.remove actorId level.Actors
        MapActors = Map.remove actor.Location level.MapActors }

let moveActor direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let movedActor = { actor with Location = targetLocation }

    { level with
        Actors = Map.add actorId movedActor level.Actors
        MapActors = level.MapActors |> Map.remove actor.Location |> Map.add targetLocation actorId }
    |> log (actor.Name + " moved")

let hurtActor damage actorId level =
    let actor = level.Actors[actorId]

    let updateHealth = Actor.mapHealth (StatValue.decreaseCurrent damage)

    { level with
        Actors = Map.mapAt actorId updateHealth level.Actors }
    |> log (actor.Name + " took damage")

// Door

let placeDoor state location level =
    { level with
        Doors = Map.add location state level.Doors }

let openDoor direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Open targetLocation |> log (actor.Name + " opened a door")

let closeDoor direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Closed targetLocation |> log (actor.Name + " closed a door")

let unlockDoor direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId

    level
    |> placeDoor Closed targetLocation
    |> log (actor.Name + " unlocked a door")

// Items

let placeItem (item: Item) location level =
    let addItem =
        Option.defaultValue []
        >> List.filter (Item.hasId item.Id >> not)
        >> List.prepend item
        >> Some

    { level with
        Items = Map.change location addItem level.Items }

let private mergeItemMaps items1 items2 =
    let folder items id item = Map.add id item items
    Map.fold folder items1 items2

let takeItems direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let locationItems = level |> itemsAt targetLocation
    let newBackpack = actor.Backpack |> mergeItemMaps (locationItems |> toItemMap)
    let newActor = { actor with Backpack = newBackpack }

    let messages =
        locationItems |> List.map (fun item -> actor.Name + " took " + item.Name)

    { level with
        Actors = Map.add actorId newActor level.Actors
        Items = Map.remove targetLocation level.Items }
    |> logAll messages
