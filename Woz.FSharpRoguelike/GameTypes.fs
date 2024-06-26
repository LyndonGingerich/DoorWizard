module GameTypes

open Aether
open Aether.Operators

open Library
open Library.Optics.Map
open Library.Optics.List

type Tile =
    | Void
    | Floor
    | Wall
    | Water

type LevelMap = Tile list list

module LevelMap =
    let bottomLeft = Vector.create 0 0
    let width (map: LevelMap) = map[0].Length - 1
    let height (map: LevelMap) = map.Length - 1

    let topRight tiles =
        Vector.create (width tiles) (height tiles)

type Door =
    | Open
    | Closed
    | Locked of string // Key name

type Stats =
    | Health
    | Strength
    | Intelligence
    | Stamina
    | Dexterity

type Stat = { Current: int; Max: int }

module Stat =
    let current_ = (_.Current), (fun current stat -> { stat with Current = current })

    let max_ = (_.Max), (fun max stat -> { stat with Max = max })

type Slot =
    | Helmet
    | Torso
    | Legs
    | Gloves
    | Boots

type Armor =
    { Slot: Slot
      Defense: int
      Absorbs: int }

type Weapon = { Attack: int; Damage: int }

type Potion = { Stat: Stats; Effect: int }

type ItemType =
    | Key
    | Armor of Armor
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
      IsNpc: bool
      Name: string
      Stats: Map<Stats, Stat>
      Location: Vector
      Backpack: Map<int, Item>
      Equipped: Map<Slot, Item>
      Weapon: Item option }

module Actor =
    let stats_ = (_.Stats), (fun stats actor -> { actor with Stats = stats })

    let expectStatFor_ stat = stats_ >-> expectValue_ stat

    let currentHealth_ = (expectStatFor_ Health) >-> Stat.current_

type Level =
    { PlayerId: int

      Map: LevelMap
      Doors: Map<Vector, Door>
      Actors: Map<int, Actor>
      Items: Map<Vector, List<Item>>

      MapActors: Map<Vector, int>

      Messages: List<string> }

module Level =
    // Doors

    let doors_ = (_.Doors), (fun doors level -> { level with Doors = doors })

    let doorAt_ location = doors_ >-> Map.value_ location

    let expectDoorAt_ location = doors_ >-> expectValue_ location

    // Actors

    let actors_ = (_.Actors), (fun actors level -> { level with Actors = actors })

    let actorWithId_ actorId = actors_ >-> Map.value_ actorId

    let expectActorWithId_ actorId = actors_ >-> expectValue_ actorId

    let mapActors_ =
        (_.MapActors), (fun mapActors level -> { level with MapActors = mapActors })

    let mapActorAt_ location = mapActors_ >-> Map.value_ location

    let expectMapActorAt_ location = mapActors_ >-> expectValue_ location

    // Items

    let items_ = (_.Items), (fun items level -> { level with Items = items })

    let itemsAt_ location =
        items_ >-> Map.value_ location >-> notEmpty_

    let itemWithId_ location id =
        itemsAt_ location >-> where_ (Item.hasId id)

    let expectItemWithId_ location id =
        itemsAt_ location >-> expectWhere_ (Item.hasId id)
