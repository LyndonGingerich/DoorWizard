module Operations

open Aether

open GameTypes
open GameTypes.Level
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
    let actor = level |> getActor actorId

    { level with
        Actors = Map.remove actorId level.Actors
        MapActors = Map.remove actor.Location level.MapActors }

let moveActor direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let movedActor = { actor with Location = targetLocation }

    level
    |> Optic.set (expectActorWithId_ actorId) movedActor
    |> Optic.set (mapActorAt_ actor.Location) None
    |> Optic.set (expectMapActorAt_ targetLocation) actorId
    |> log (actor.Name + " moved")

let hurtActor damage actorId level =
    let actor = level |> Optic.get (expectActorWithId_ actorId)

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

let placeItem (item: Item) location =
    Optic.set (itemWithId_ location item.Id) (Some item)

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

    level
    |> Optic.set (expectActorWithId_ actorId) newActor
    |> Optic.set (itemsAt_ targetLocation) []
    |> logAll messages
