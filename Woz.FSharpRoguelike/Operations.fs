module Operations

open Aether

open GameTypes
open GameTypes.Level
open Library
open Queries.Level

// Messages

let log message level =
    let messages = message :: level.Messages
    { level with Messages = messages }

let logAll newMessages level =
    let messages = level.Messages |> Seq.append newMessages |> List.ofSeq
    { level with Messages = messages }

let flush level = { level with Messages = [] }

// Stats

let private decreaseCurrent amount stat =
    { stat with
        Current = max (stat.Current - amount) 0 }

// Actor

let actorTarget direction actorId level =
    let actor = level |> expectActor actorId
    let targetLocation = actor.Location + direction
    actor, targetLocation

let spawnActor actor level =
    { level with
        Actors = Map.add actor.Id actor level.Actors
        MapActors = Map.add actor.Location actor.Id level.MapActors }

let removeActor actorId level =
    let actor = level |> expectActor actorId

    level
    |> Optic.set (actorWithId_ actorId) None
    |> Optic.set (mapActorAt_ actor.Location) None

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

    let updateHealth = Actor.mapHealth (decreaseCurrent damage)

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
        locationItems |> Seq.map (fun item -> actor.Name + " took " + item.Name)

    level
    |> Optic.set (expectActorWithId_ actorId) newActor
    |> Optic.set (itemsAt_ targetLocation) []
    |> logAll messages
