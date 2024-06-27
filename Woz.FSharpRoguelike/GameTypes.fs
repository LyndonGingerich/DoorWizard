module GameTypes

open Library

type Tile =
    | Void
    | Floor
    | Wall
    | Water

type LevelMap = Tile list list

module LevelMap =
    let lastRow = 11
    let lastColumn = 43
    let bottomLeft = Vector.create 0 0
    let topRight = Vector.create lastColumn lastRow

type Door =
    | Open
    | Closed
    | Locked of string // Key name

type Stat =
    | Health
    | Strength
    | Intelligence
    | Stamina
    | Dexterity

type StatValue = { Current: int; Max: int }

module StatValue =
    let decreaseCurrent amount m =
        { m with
            Current = m.Current - amount |> max 0 }

type Stats = { Health: StatValue }

type Weapon = { Attack: int; Damage: int }

type ItemType =
    | Key
    | Potion

type Item = { Type: ItemType; Name: string }

type Actor =
    { Id: int
      Name: string
      Stats: Stats
      Location: Vector
      WieldedItem: Item option }

module Actor =
    let mapHealth f actor =
        { actor with
            Actor.Stats.Health = f actor.Stats.Health }

type Level =
    { Map: LevelMap
      Doors: Map<Vector, Door>
      Actors: Map<int, Actor>
      Items: Map<Vector, List<Item>>
      MapActors: Map<Vector, int>
      Messages: List<string> }

module Level =
    let popMessage m =
        match m.Messages with
        | head :: tail -> Some head, { m with Messages = tail }
        | [] -> None, m

let playerId = 1
