module Queries

open Microsoft.FSharp.Core.Option

open GameTypes
open Library

module Level =
    let hasCoordinate location level =
        location >= LevelMap.bottomLeft && location <= LevelMap.topRight level.Map

    let getTile location level = level.Map[location.y][location.x]

    let isPlayerId level = (=) level.PlayerId
    let isNpcId level = (<>) level.PlayerId

    let actorIds level = level.Actors.Keys

    let npcIds level =
        let isNpc = isNpcId level
        level |> actorIds |> Seq.filter isNpc

    let hasActor location level =
        level.MapActors |> Map.tryFind location |> isSome

    let hasKey keyName actor =
        actor.Backpack.Values
        |> Seq.filter (fun item -> item.Type = Key)
        |> Seq.exists (fun i -> i.Name = keyName)

    let findActor actorId level = level.Actors |> Map.tryFind actorId

    let getActor actorId level = level.Actors[actorId]

    let isDead actor = actor.Stats.Health.Current = 0

    let isPlayerDead level =
        level |> getActor level.PlayerId |> isDead

    let findDoor location level = level.Doors |> Map.tryFind location

    let expectDoor location level = level.Doors[location]

    let hasDoor location level = level |> findDoor location |> isSome

    let private isBlockingTile location level =
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
    let private blockMove = [ isBlockingTile; isBlockingDoor; hasActor ]

    let private checkLocationFor predicates location level =
        if hasCoordinate location level then
            let testPredicate = (fun predicate -> level |> predicate location)
            predicates |> Seq.exists testPredicate
        else
            true

    let locationBlocksView = checkLocationFor blockView

    let locationBlocksMove = checkLocationFor blockMove

    let itemsAt location level =
        level.Items |> Map.tryFind location |> Option.defaultValue []

    let toItemMap = List.map (fun (item: Item) -> (item.Id, item)) >> Map.ofList

    let hasItems location = itemsAt location >> Seq.isEmpty >> not

    let findItem location id =
        itemsAt location >> List.tryFind (Item.hasId id)

    let expectItem location id =
        itemsAt location >> List.find (Item.hasId id)
