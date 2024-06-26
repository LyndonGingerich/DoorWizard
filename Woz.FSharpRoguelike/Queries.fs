﻿module Queries

open Microsoft.FSharp.Core.Option
open Aether

open GameTypes
open GameTypes.Actor
open GameTypes.ItemType
open GameTypes.Level
open GameTypes.Map
open Library

module Level =
    let hasCoordinate location level =
        location >= Map.bottomLeft && location <= topRight level.map

    let getTile location level = level.map.[location.y].[location.x]

    let isPlayerId level = (=) level.playerId
    let isNpcId level = (<>) level.playerId

    let actorIds level =
        level.actors |> Map.toSeq |> Seq.map fst

    let npcIds level =
        let isNpc = isNpcId level
        level |> actorIds |> Seq.filter isNpc

    let hasActor location level =
        level |> Optic.get (mapActorAt_ location) |> isSome

    let hasKey keyName actor =
        actor.backpack
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (_.Type >> isKey)
        |> Seq.exists (fun i -> i.Name = keyName)

    let findActor actorId = Optic.get (actorWithId_ actorId)

    let expectActor actorId = Optic.get (expectActorWithId_ actorId)

    let isDead actor = actor |> Optic.get currentHealth_ = 0

    let isPlayerDead level =
        level |> expectActor level.playerId |> isDead

    let findDoor location = Optic.get (doorAt_ location)

    let expectDoor location = Optic.get (expectDoorAt_ location)

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

    let itemsAt location = Optic.get (itemsAt_ location)

    let toItemMap = List.map (fun item -> (item.Id, item)) >> Map.ofList

    let hasItems location = itemsAt location >> Seq.isEmpty >> not

    let findItem location id = Optic.get (itemWithId_ location id)

    let expectItem location id =
        Optic.get (expectItemWithId_ location id)
