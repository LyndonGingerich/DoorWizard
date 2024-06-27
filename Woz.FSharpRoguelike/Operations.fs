module Operations

open GameTypes
open Library
open Queries.Level

// Messages

let logAll newMessages level =
    { level with
        Level.Messages = level.Messages @ newMessages }

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

let moveActor targetLocation actor level =
    let movedActor = { actor with Location = targetLocation }

    { level with
        Actors = Map.add actor.Id movedActor level.Actors
        MapActors = level.MapActors |> Map.remove actor.Location |> Map.add targetLocation actor.Id }

let hurtActor damage actorId level =
    let updateHealth = Actor.mapHealth (StatValue.decreaseCurrent damage)

    { level with
        Actors = Map.mapAt actorId updateHealth level.Actors }

let move vector level =
    let player = level.Actors[playerId]
    let newLocation = player.Location + vector

    if isBlockingTile newLocation level then
        level, []
    else
        match Map.tryFind newLocation level.Doors with
        | None
        | Some Open -> moveActor newLocation player level, []
        | Some Closed ->
            { level with
                Doors = Map.add newLocation Open level.Doors },
            [ "The door opens." ]
        | Some(Locked keyName) ->
            match player.WieldedItem with
            | Some { Type = Key; Name = itemName } ->
                if itemName = keyName then
                    { level with
                        Doors = Map.add newLocation Closed level.Doors
                        Actors = Map.add playerId (Actor.expendItem player) level.Actors },
                    [ "You use your key to unlock the door." ]
                else
                    level, [ $"You scratch futilely at the {keyName} door with your {itemName} key." ]
            | Some _
            | None -> level, [ "The door is locked." ]

// Door

let placeDoor state location level =
    { level with
        Doors = Map.add location state level.Doors }

let openDoor direction actorId level =
    let _, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Open targetLocation

let closeDoor direction actorId level =
    let _, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Closed targetLocation

let unlockDoor direction actorId level =
    let _, targetLocation = level |> actorTarget direction actorId
    level |> placeDoor Closed targetLocation

// Items

let placeItem (item: Item) location level =
    let addItem = Option.defaultValue [] >> List.prepend item >> Some

    { level with
        Items = Map.change location addItem level.Items }

let takeItems direction actorId level =
    let actor, targetLocation = level |> actorTarget direction actorId
    let locationItems = level |> itemsAt targetLocation
    let newWielded = locationItems.Head
    let newLocationItems = locationItems.Tail @ Option.toList actor.WieldedItem

    let newActor =
        { actor with
            WieldedItem = Some newWielded }

    let message =
        let pickUp = $"You pick up %s{newWielded.Name}."

        match actor.WieldedItem with
        | Some oldWielded -> $"%s{pickUp} You drop {oldWielded.Name}."
        | None -> pickUp

    { level with
        Actors = Map.add actorId newActor level.Actors
        Items = Map.add targetLocation newLocationItems level.Items }
    |> logAll [ message ]
