module RenderEngine

open System

open GameTypes
open GameTypes.LevelMap
open Library
open Queries.Level

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

let private renderTile level location =
    let char =
        [ maybeActor; maybeItems; maybeDoor; maybeTile ]
        |> List.tryPick (fun f -> f location level)

    match char with
    | Some c -> c
    | None -> ' '

let private xs = seq { 0 .. topRight.x }

let private ys = seq { (topRight.y - 1) .. -1 .. 0 }

let render level =
    let buildRow currentY =
        xs
        |> Seq.map (fun nextX -> Vector.create nextX currentY)
        |> Seq.map (renderTile level)
        |> Seq.toArray
        |> String

    let print strings =
        strings |> Seq.iter (printfn "%s")
        printfn ""

    Console.Clear()

    ys |> Seq.map buildRow |> print
    level.Messages |> List.rev |> print

    level
