module RenderEngine

open System
open Aether

open GameTypes
open GameTypes.Level
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

let private maybeActor location =
    Optic.get (mapActorAt_ location) >> Option.map (fun actorId -> '@')

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

let private xs map = seq { 0 .. (topRight map).x }

let private ys map =
    seq { ((topRight map).y - 1) .. -1 .. 0 }

let render level =
    let buildRow map currentY =
        xs map
        |> Seq.map (fun nextX -> Vector.create nextX currentY)
        |> Seq.map (renderTile level)
        |> Seq.toArray
        |> System.String

    let print strings =
        strings |> Seq.iter (printfn "%s")
        printfn ""

    Console.Clear()

    ys level.Map |> Seq.map (buildRow level.Map) |> print
    level.Messages |> List.rev |> print

    level
