[<RequireQualifiedAccess>]
module Die

open System

let roll random count size () =
    let next (random: Random) = random.Next(1, size + 1)
    next |> List.replicate count |> List.sumBy (fun f -> f random)
