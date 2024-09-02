module DoorWizard.Engine.Commands

open GameTypes
open Operations

let buildCommand validator operation direction level =
    match validator direction playerId level with
    | Some message -> (level, [ message ])
    | None -> operation direction playerId level, []

let useDoorMagic command direction level =
    let player = level.Actors[playerId]

    let rec inner location level =
        let newLocation = location + direction

        if Queries.isBlockingTile newLocation level then
            level
        else
            level |> command newLocation |> inner newLocation

    inner player.Location level

let doorBeamCommand direction level =
    useDoorMagic (placeDoor Open) direction level, [ "kaploop!" ]

let doorStopperCommand direction level =
    useDoorMagic removeDoor direction level, [ "poolhcs!" ]

let doorBoltCommand direction level =
    useDoorMagic (placeDoor (Locked "cat")) direction level, [ "ching ching!" ]

let doorBlastCommand direction level =
    useDoorMagic (placeDoor Closed) direction level, [ "Schloop!" ]
