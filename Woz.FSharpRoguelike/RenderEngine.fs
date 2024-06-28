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

let private maybeHealthDisplay { x = x; y = y } level =
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

let private xs = seq { 0 .. topRight.x }

let private ys = seq { (topRight.y - 1) .. -1 .. 0 }

let printAll strings =
    strings |> Seq.iter (printfn "%s")
    printfn ""

let print string = printfn $"%s{string}\n"

let printNextMessage level =
    let maybeMessage, level = Level.popMessage level

    match maybeMessage with
    | Some message ->
        let message =
            if not level.Messages.IsEmpty then
                $"{message}--More--"
            else
                message

        print message
    | None -> ()

    level

let render level =
    let buildRow currentY =
        xs
        |> Seq.map (fun nextX -> Vector.create nextX currentY)
        |> Seq.map (renderTile level)
        |> Seq.toArray
        |> String

    Console.Clear()

    ys |> Seq.map buildRow |> printAll

    let level = printNextMessage level

    level
