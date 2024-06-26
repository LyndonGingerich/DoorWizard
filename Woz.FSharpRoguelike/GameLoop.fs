module GameLoop

open Commands
open GameTypes
open Library
open PlayerInput
open Operations
open Queries.Level
open RenderEngine

// Stub
let private getAiCommand actorId = idleCommand

let private runAiCommand level command =
    (command level).Contents |> Option.defaultValue level

let private runAi (level: Level) =
    level
    |> npcIds
    |> Seq.map getAiCommand
    |> Seq.fold runAiCommand level
    |> OperationResult.success

let private runTurn playerCommand level =
    let turnResult =
        result {
            let! playerMoved = level |> playerCommand
            let! aiMoved = playerMoved |> runAi
            return aiMoved
        }

    turnResult.Contents |> Option.defaultValue level |> logAll turnResult.Messages

let rec gameLoop level =
    let playerCommand = getPlayerCommand ()
    let turnLevel = level |> runTurn playerCommand |> render |> flush

    if turnLevel |> isPlayerDead then
        ()
    else
        turnLevel |> gameLoop
