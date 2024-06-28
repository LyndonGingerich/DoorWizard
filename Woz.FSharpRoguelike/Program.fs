module Program

open System

open GameLoop
open LevelFactory

[<EntryPoint>]
let main argv =
    testLevel (Random()) |> gameLoop
    0 // return an integer exit code
