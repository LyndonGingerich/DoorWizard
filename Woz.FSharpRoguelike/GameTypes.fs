﻿module GameTypes

open Aether
open Aether.Operators

open Library
open Library.Optics.Map
open Library.Optics.List

// -----------------------------------------

type Tile =
    | Void
    | Floor
    | Wall
    | Water

type LevelMap = Tile list list

module Map =
    let bottomLeft = Vector.create 0 0
    let width (map: LevelMap) = map[0].Length - 1
    let height (map: LevelMap) = map.Length - 1

    let topRight tiles =
        Vector.create (width tiles) (height tiles)

// -----------------------------------------

type Door =
    | Open
    | Closed
    | Locked of string // Key name

// -----------------------------------------

type Stats =
    | Health
    | Strength
    | Intelligence
    | Stamina
    | Dexterity

type Stat = { current: int; max: int }

module Stat =
    let current_ = (_.current), (fun current stat -> { stat with current = current })

    let max_ = (_.max), (fun max stat -> { stat with max = max })

// -----------------------------------------

type Slot =
    | Helmet
    | Torso
    | Legs
    | Gloves
    | Boots

type Key = { id: int; name: string }

type Armor =
    { id: int
      name: string
      slot: Slot
      defense: int
      absorbs: int }

type Weapon =
    { id: int
      name: string
      attack: int
      damage: int }

type Potion =
    { id: int
      name: string
      stat: Stats
      effect: int }

type Item =
    | Key of Key
    | Armor of Armor
    | Weapon of Weapon
    | Potion of Potion

module Item =
    let idOf item =
        match item with
        | Key k -> k.id
        | Armor a -> a.id
        | Weapon w -> w.id
        | Potion p -> p.id

    let nameOf item =
        match item with
        | Key k -> k.name
        | Armor a -> a.name
        | Weapon w -> w.name
        | Potion p -> p.name

    let hasId id item = (idOf item) = id

    let isKey item =
        match item with
        | Key _ -> true
        | _ -> false

// -----------------------------------------

type Actor =
    { id: int
      isNpc: bool
      name: string
      stats: Map<Stats, Stat>
      location: Vector
      backpack: Map<int, Item>
      equipped: Map<Slot, Item>
      weapon: Item option }

module Actor =
    let stats_ = (_.stats), (fun stats actor -> { actor with stats = stats })

    let statFor_ stat = stats_ >-> Map.value_ stat

    let expectStatFor_ stat = stats_ >-> expectValue_ stat

    let currentHealth_ = (expectStatFor_ Health) >-> Stat.current_

    let location_ =
        (_.location), (fun location actor -> { actor with location = location })

    let backpack_ =
        (_.backpack), (fun backpack actor -> { actor with backpack = backpack })

    let backpackItemWithId_ id = backpack_ >-> Map.value_ id

    let expectBackpackItemWithId_ id = backpack_ >-> expectValue_ id

// -----------------------------------------

type Level =
    { playerId: int

      map: LevelMap
      doors: Map<Vector, Door>
      actors: Map<int, Actor>
      items: Map<Vector, List<Item>>

      mapActors: Map<Vector, int>

      messages: List<string> }

module Level =
    // Doors

    let doors_ = (_.doors), (fun doors level -> { level with doors = doors })

    let doorAt_ location = doors_ >-> Map.value_ location

    let expectDoorAt_ location = doors_ >-> expectValue_ location

    // Actors

    let actors_ = (_.actors), (fun actors level -> { level with actors = actors })

    let actorWithId_ actorId = actors_ >-> Map.value_ actorId

    let expectActorWithId_ actorId = actors_ >-> expectValue_ actorId

    let mapActors_ =
        (_.mapActors), (fun mapActors level -> { level with mapActors = mapActors })

    let mapActorAt_ location = mapActors_ >-> Map.value_ location

    let expectMapActorAt_ location = mapActors_ >-> expectValue_ location

    // Items

    let items_ = (_.items), (fun items level -> { level with items = items })

    let itemsAt_ location =
        items_ >-> Map.value_ location >-> notEmpty_

    let itemWithId_ location id =
        itemsAt_ location >-> where_ (Item.hasId id)

    let expectItemWithId_ location id =
        itemsAt_ location >-> expectWhere_ (Item.hasId id)

    // Messages

    let messages_ =
        (_.messages), (fun messages level -> { level with messages = messages })
