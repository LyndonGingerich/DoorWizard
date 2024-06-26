﻿module GameTypes

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
    let current_ = (_.Current), (fun current stat -> { stat with Current = current })

    let max_ = (_.Max), (fun max stat -> { stat with Max = max })

    let decreaseCurrent amount m =
        { m with
            Current = m.Current - amount |> max 0 }

type Stats = { Health: StatValue }

type Weapon = { Attack: int; Damage: int }

type Potion = { Stat: Stat; Effect: int }

type ItemType =
    | Key
    | Weapon of Weapon
    | Potion of Potion

type Item =
    { Type: ItemType
      Name: string
      Id: int }

module Item =
    let hasId id m = m.Id = id

type Actor =
    { Id: int
      Name: string
      Stats: Stats
      Location: Vector
      Backpack: Map<int, Item> }

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

let playerId = 1
