module GameLoop

open System

open PlayerInput
open Operations
open Queries
open RenderEngine

let private runTurn playerCommand level =
    let newLevel, messages = level |> playerCommand
    newLevel |> logAll messages

let render level =
    Console.Clear()

    printAll (buildLevel level)

    let level, maybeMessage = getMaybeNextMessage level
    Option.iter print maybeMessage

    level

let rec gameLoop level =
    let turnLevel = level |> runTurn handleKeyPress |> render

    if turnLevel |> isPlayerDead then
        ()
    else
        turnLevel |> gameLoop
