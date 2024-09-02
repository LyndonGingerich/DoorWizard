[<RequireQualifiedAccess>]
module DoorWizard.Library.Map

let mapAt key f m =
    match Map.tryFind key m with
    | Some value -> Map.add key (f value) m
    | None -> m
