module DoorWizard.Engine.Queries

open GameTypes

let hasCoordinate location =
    location >= LevelMap.bottomLeft && location <= LevelMap.topRight

let getTile location level = level.Map[location.Y][location.X]

let isPlayerId id = id = playerId
let isNpcId id = id <> playerId

let actorIds level = level.Actors.Keys

let npcIds level = level |> actorIds |> Seq.filter isNpcId

let hasKey keyName actor =
    match actor.WieldedItem with
    | Some { Type = Key; Name = wieldedName } -> wieldedName = keyName
    | _ -> false

let getActor actorId level = level.Actors[actorId]

let isDead actor = actor.Stats.Health.Current = 0

let isPlayerDead level = level |> getActor playerId |> isDead

let findDoor location level = level.Doors |> Map.tryFind location

let expectDoor location level = level.Doors[location]

let hasDoor location level =
    level |> findDoor location |> Option.isSome

let isBlockingTile location level =
    match level |> getTile location with
    | Void
    | Wall -> true
    | Floor
    | Water -> false

let private isBlockingDoor location level =
    match level |> findDoor location with
    | Some door ->
        match door with
        | Closed
        | Locked _ -> true
        | Open -> false
    | None -> false

let private blockView = [ isBlockingTile; isBlockingDoor ]

let private checkLocationFor predicates location level =
    if hasCoordinate location then
        let testPredicate = (fun predicate -> level |> predicate location)
        predicates |> Seq.exists testPredicate
    else
        true

let locationBlocksView = checkLocationFor blockView

let itemsAt location level =
    level.Items |> Map.tryFind location |> Option.defaultValue []

let hasItems location = itemsAt location >> Seq.isEmpty >> not
