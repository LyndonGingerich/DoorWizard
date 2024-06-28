module GameLoop

open Library
open PlayerInput
open Operations
open Queries.Level
open RenderEngine

let private runTurn playerCommand level =
    let turnResult = level |> playerCommand
    turnResult.Contents |> Option.defaultValue level |> logAll turnResult.Messages

let rec gameLoop level =
    let turnLevel = level |> runTurn (getPlayerCommand ()) |> render

    if turnLevel |> isPlayerDead then
        ()
    else
        turnLevel |> gameLoop
