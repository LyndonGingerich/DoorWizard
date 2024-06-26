module LevelFactory

open GameTypes
open Library
open Operations

let private testLevelTemplate =
    [ "                                            " // y = 0
      "    ################                        " // y = 1
      "    #..............#         ###########    " // y = 2
      "    #..............#         #......~~~#    " // y = 3
      "    #..............###########......~~~#    " // y = 4
      "    #................................###    " // y = 5
      "    #..............###########.......#      " // y = 6
      "    #..............#         #########      " // y = 7
      "    ################                        " // y = 8
      "                                            " // y = 9
      "                                            " // y = 10
      "                                            " ] // y = 11

let private testPlayer =
    { Id = 1
      Name = "player"
      Stats = { Health = { Current = 10; Max = 10 } }
      Location = Vector.create 9 6
      Backpack = Map.empty<int, Item> }

let private charToTile character =
    match character with
    | '#' -> Wall
    | '.' -> Floor
    | '~' -> Water
    | _ -> Void

let private basicKey = "Basic key"

let private testItem1 = { Type = Key; Id = 2; Name = basicKey }

let private testItem2 =
    { Id = 3
      Name = "Minor health potion"
      Type = Potion { Stat = Health; Effect = 5 } }

let private testMap =
    let rowToTiles row = row |> Seq.map charToTile |> List.ofSeq

    testLevelTemplate |> Seq.rev |> Seq.map rowToTiles |> List.ofSeq

let testLevel =
    let level =
        { PlayerId = testPlayer.Id
          Map = testMap
          Doors = Map.empty<Vector, Door>
          Actors = Map.empty<int, Actor>
          Items = Map.empty<Vector, List<Item>>
          MapActors = Map.empty<Vector, int>
          Messages = [] }

    level
    |> spawnActor testPlayer
    |> placeDoor Open (Vector.create 19 6)
    |> placeDoor Closed (Vector.create 29 6)
    |> placeDoor (Locked basicKey) (Vector.create 24 6)
    |> placeItem testItem1 (Vector.create 17 5)
    |> placeItem testItem2 (Vector.create 16 7)
