module GameLoop

open PlayerInput
open Operations
open Queries
open RenderEngine

let private runTurn playerCommand level =
    let newLevel, messages = level |> playerCommand
    newLevel |> logAll messages

let rec gameLoop level =
    let turnLevel = level |> runTurn handleKeyPress |> render

    if turnLevel |> isPlayerDead then
        ()
    else
        turnLevel |> gameLoop
