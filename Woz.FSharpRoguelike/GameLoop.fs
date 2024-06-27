﻿module GameLoop

open Commands
open GameTypes
open Library
open PlayerInput
open Operations
open Queries.Level
open RenderEngine
open Result

// Stub
let private getAiCommand actorId = idleCommand

let private runAiCommand level command =
    match level |> command with
    | Ok updatedLevel -> updatedLevel
    | Error _ -> level

let private runAi (level: Level) =
    level |> npcIds |> Seq.map getAiCommand |> Seq.fold runAiCommand level |> Ok

let private runTurn playerCommand level =
    let turnResult =
        result {
            let! playerMoved = level |> playerCommand
            let! aiMoved = playerMoved |> runAi
            return aiMoved
        }

    match turnResult with
    | Ok turnLevel -> turnLevel
    | Error message -> level |> log message

let rec gameLoop level =
    let playerCommand = getPlayerCommand ()
    let turnLevel = level |> runTurn playerCommand |> render |> flush

    if turnLevel |> isPlayerDead then
        ()
    else
        turnLevel |> gameLoop
