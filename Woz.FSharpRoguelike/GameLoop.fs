module GameLoop

open System

open PlayerInput
open Queries
open RenderEngine

let printAll strings =
    strings |> Seq.iter (printfn "%s")
    printfn ""

let print string = printfn $"%s{string}\n"

let render level =
    Console.Clear()

    printAll (buildLevel level)

    let level, maybeMessage = getMaybeNextMessage level
    Option.iter print maybeMessage

    level

let rec gameLoop level =
    let turnLevel = level |> render |> runTurn (handleKeyPress (Console.ReadKey().Key))

    if turnLevel |> isPlayerDead then
        print deathMessage
    else
        turnLevel |> gameLoop
