module Program

open System

[<EntryPoint>]
let main argv =
    LevelFactory.testLevel (Random()) |> GameLoop.gameLoop
    0 // return an integer exit code
