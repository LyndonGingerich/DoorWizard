module RenderEngine

open System

open GameTypes
open GameTypes.LevelMap
open Library
open Queries

let private tileToChar tile =
    match tile with
    | Wall -> '#'
    | Floor -> '.'
    | Water -> '~'
    | _ -> ' '

let private doorToChar door =
    match door with
    | Open -> '-'
    | Closed -> '+'
    | Locked _ -> '+'

let private maybeActor location level =
    level.MapActors |> Map.tryFind location |> Option.map (fun actorId -> '@')

let private maybeItems location =
    hasItems location
    >> function
        | true -> Some '?'
        | false -> None

let private maybeDoor location =
    findDoor location >> Option.map doorToChar

let private maybeTile location = getTile location >> tileToChar >> Some

let private maybeHealthDisplay { X = x; Y = y } level =
    $"Health: %i{level.Actors[playerId].Stats.Health.Current}"
    |> Seq.indexed
    |> Seq.tryFind (fun (i, _) -> x = (i + 29) && y = 1)
    |> Option.map snd

let private renderTile level location =
    let char =
        [ maybeHealthDisplay; maybeActor; maybeItems; maybeDoor; maybeTile ]
        |> List.tryPick (fun f -> f location level)

    match char with
    | Some c -> c
    | None -> ' '

let buildLevel level =
    let buildRow level currentY =
        xs
        |> Seq.map (fun nextX -> Vector.create nextX currentY)
        |> Seq.map (renderTile level)
        |> Seq.toArray
        |> String

    ys |> Seq.map (buildRow level)

let getMaybeNextMessage level =
    let maybeMessage, level = Level.popMessage level

    let maybeMessage =
        maybeMessage
        |> Option.map (fun message ->
            if not level.Messages.IsEmpty then
                $"{message}--More--"
            else
                message)

    level, maybeMessage
