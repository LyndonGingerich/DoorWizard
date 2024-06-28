module Commands

open GameTypes
open Library
open Operations

let buildCommand (validator: Vector -> int -> Level -> _) (operation: Vector -> int -> Level -> Level) direction level =
    result {
        let! validLevel = validator direction playerId level
        return operation direction playerId validLevel
    }

let movePlayer move level =
    Operations.move move level |> OperationResult.ofTuple

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
    OperationResult.ofTuple (useDoorMagic (placeDoor Open) direction level, [ "Schloop!" ])

let doorStopperCommand direction level =
    OperationResult.ofTuple (useDoorMagic removeDoor direction level, [ "poolhcs!" ])

let doorBoltCommand direction level =
    OperationResult.ofTuple (useDoorMagic (placeDoor (Locked "cat")) direction level, [ "ching ching!" ])

let doorBeamCommand direction level =
    OperationResult.ofTuple (useDoorMagic (placeDoor Closed) direction level, [ "KapLoop!" ])
