module Commands

open GameTypes
open Operations

let buildCommand validator operation direction level =
    match validator direction playerId level with
    | Some message -> (level, [ message ])
    | None -> operation direction playerId level, []

let movePlayer move level = Operations.move move level

let useDoorMagic command direction level =
    let player = level.Actors[playerId]

    let rec inner location level =
        let newLocation = location + direction

        if Queries.Level.isBlockingTile newLocation level then
            level
        else
            level |> command newLocation |> inner newLocation

    inner player.Location level

let doorBlastCommand direction level =
    useDoorMagic (placeDoor Open) direction level, [ "Schloop!" ]

let doorStopperCommand direction level =
    useDoorMagic removeDoor direction level, [ "poolhcs!" ]

let doorBoltCommand direction level =
    useDoorMagic (placeDoor (Locked "cat")) direction level, [ "ching ching!" ]

let doorBeamCommand direction level =
    useDoorMagic (placeDoor Closed) direction level, [ "KapLoop!" ]
