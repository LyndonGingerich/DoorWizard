[<RequireQualifiedAccess>]
module DieRoll

open System

let private next maxValue (random: Random) = random.Next(1, maxValue)

let d count size () =
    let random = Random()
    next (size + 1) |> List.replicate count |> List.sumBy (fun f -> f random)
