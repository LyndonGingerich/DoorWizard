module DoorWizard.Console.Program

open System

open DoorWizard.Engine

let printAll strings =
    strings |> Seq.iter (printfn "%s")
    printfn ""

let print string = printfn $"%s{string}\n"

let render level =
    Console.Clear()

    printAll (RenderEngine.buildLevel level)

    let level, maybeMessage = RenderEngine.getMaybeNextMessage level
    Option.iter print maybeMessage

    level

let rec gameLoop level =
    let turnLevel =
        level
        |> render
        |> PlayerInput.runTurn (PlayerInput.handleKeyPress (Console.ReadKey().Key))

    if turnLevel |> Queries.isPlayerDead then
        print PlayerInput.deathMessage
    else
        turnLevel |> gameLoop

[<EntryPoint>]
let main argv =
    LevelFactory.testLevel |> gameLoop
    0 // return an integer exit code
