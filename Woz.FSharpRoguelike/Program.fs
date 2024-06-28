module Program

open System

open GameLoop
open RenderEngine
open LevelFactory

[<EntryPoint>]
let main argv =
    render (testLevel (Random())) |> gameLoop
    0 // return an integer exit code
