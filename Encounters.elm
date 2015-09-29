module Encounters where

import Dict exposing (Dict)
import Maybe

import Dist exposing (Dist)
import Pokemon exposing (..)

baseBattleLength = 549
framesBeforeMove = 44

battleLength : Species -> Species -> Int
battleLength you enemy =
    baseBattleLength + you.cryDiff + enemy.cryDiff

type alias Encounter =
    { species : Species
    , level : Int
    }

slots =
    [ (0, 51)
    , (51, 51)
    , (102, 39)
    , (141, 25)
    , (166, 25)
    , (191, 25)
    , (216, 13)
    , (229, 13)
    , (242, 11)
    , (253, 3)
    ]
slotStarts = List.map fst slots
slotWidths = List.map snd slots

slotFromRand : Int -> Int
slotFromRand x = let x' = x % 256 in if
    | x < 51 -> 1
    | x < 102 -> 2
    | x < 141 -> 3
    | x < 166 -> 4
    | x < 191 -> 5
    | x < 216 -> 6
    | x < 229 -> 7
    | x < 242 -> 8
    | x < 253 -> 9
    | otherwise -> 10

type alias EncounterTable =
    { name : String
    , rate : Int
    , slot1 : Encounter
    , slot2 : Encounter
    , slot3 : Encounter
    , slot4 : Encounter
    , slot5 : Encounter
    , slot6 : Encounter
    , slot7 : Encounter
    , slot8 : Encounter
    , slot9 : Encounter
    , slot10 : Encounter
    }

encounter : String -> Int -> Encounter
encounter name level = 
    { species = speciesByName
        |> Dict.get name
        |> Maybe.withDefault noSpecies
    , level = level
    }

displayName : Encounter -> String
displayName enc =
    "L" ++ toString enc.level ++ " " ++ enc.species.name

noEncounter : Encounter
noEncounter = { species = noSpecies, level = 0 }

allTables : List EncounterTable
allTables =
    [ route1table
    , route2table
    , route3table
    , route4table
    , route5table
    , route6table
    , route7table
    , route8table
    , route9table
    , route10table
    , route11table
    , route12table
    , route13table
    , route14table
    , route15table
    , route16table
    , route17table
    , route18table
    , seaRoute19waterTable
    , seaRoute20waterTable
    , seaRoute21table
    , seaRoute21waterTable
    , route22table
    , route23table
    , route24table
    , route25table
    , viridianForestTable
    , mtMoonTable1
    , mtMoonTable2
    , mtMoonTable3
    , rockTunnelTable1
    , rockTunnelTable2
    , powerPlantTable
    , victoryRoadTable1
    , victoryRoadTable2
    , victoryRoadTable3
    , pokemonTowerTable1
    , pokemonTowerTable2
    , pokemonTowerTable3
    , pokemonTowerTable4
    , pokemonTowerTable5
    , seafoamIslandsTable1
    , seafoamIslandsTable2
    , seafoamIslandsTable3
    , seafoamIslandsTable4
    , seafoamIslandsTable5
    , pokemonMansionTable1
    , pokemonMansionTable2
    , pokemonMansionTable3
    , pokemonMansionTable4
    , diglettsCaveTable
    , safariZoneTable1
    , safariZoneTable2
    , safariZoneTable3
    , safariZoneTable4
    , ceruleanCaveTable1
    , ceruleanCaveTable2
    , ceruleanCaveTable3
    ]

defaultTable : EncounterTable
defaultTable = route1table

templateTable : EncounterTable
templateTable =
    { name = ""
    , rate = 0
    , slot1 = encounter "" 0
    , slot2 = encounter "" 0
    , slot3 = encounter "" 0
    , slot4 = encounter "" 0
    , slot5 = encounter "" 0
    , slot6 = encounter "" 0
    , slot7 = encounter "" 0
    , slot8 = encounter "" 0
    , slot9 = encounter "" 0
    , slot10 = encounter "" 0
    }

-- Encounter tables for red
-- TODO: Add other tables
-- TODO: Include blue tables
route1table : EncounterTable
route1table =
    { name = "Route 1"
    , rate = 25
    , slot1 = encounter "Pidgey" 3
    , slot2 = encounter "Rattata" 3
    , slot3 = encounter "Rattata" 3
    , slot4 = encounter "Rattata" 2
    , slot5 = encounter "Pidgey" 2
    , slot6 = encounter "Pidgey" 3
    , slot7 = encounter "Pidgey" 3
    , slot8 = encounter "Rattata" 4
    , slot9 = encounter "Pidgey" 4
    , slot10 = encounter "Pidgey" 5
    }

route2table : EncounterTable
route2table =
    { name = "Route 2"
    , rate = 25
    , slot1 = encounter "Rattata" 3
    , slot2 = encounter "Pidgey" 3
    , slot3 = encounter "Pidgey" 4
    , slot4 = encounter "Rattata" 4
    , slot5 = encounter "Pidgey" 5
    , slot6 = encounter "Weedle" 3
    , slot7 = encounter "Rattata" 2
    , slot8 = encounter "Rattata" 5
    , slot9 = encounter "Weedle" 4
    , slot10 = encounter "Weedle" 5
    }

route3table : EncounterTable
route3table =
    { name = "Route 3"
    , rate = 20
    , slot1 = encounter "Pidgey" 6
    , slot2 = encounter "Spearow" 5
    , slot3 = encounter "Pidgey" 7
    , slot4 = encounter "Spearow" 6
    , slot5 = encounter "Spearow" 7
    , slot6 = encounter "Pidgey" 8
    , slot7 = encounter "Spearow" 8
    , slot8 = encounter "Jigglypuff" 3
    , slot9 = encounter "Jigglypuff" 5
    , slot10 = encounter "Jigglypuff" 7
    }

route4table : EncounterTable
route4table =
    { name = "Route 4"
    , rate = 20
    , slot1 = encounter "Rattata" 10
    , slot2 = encounter "Spearow" 10
    , slot3 = encounter "Rattata" 8
    , slot4 = encounter "Ekans" 6
    , slot5 = encounter "Spearow" 8
    , slot6 = encounter "Ekans" 10
    , slot7 = encounter "Rattata" 12
    , slot8 = encounter "Spearow" 12
    , slot9 = encounter "Ekans" 8
    , slot10 = encounter "Ekans" 12
    }

route5table : EncounterTable
route5table =
    { name = "Route 5"
    , rate = 15
    , slot1 = encounter "Oddish" 13
    , slot2 = encounter "Pidgey" 13
    , slot3 = encounter "Pidgey" 15
    , slot4 = encounter "Mankey" 10
    , slot5 = encounter "Mankey" 12
    , slot6 = encounter "Oddish" 15
    , slot7 = encounter "Oddish" 16
    , slot8 = encounter "Pidgey" 16
    , slot9 = encounter "Mankey" 14
    , slot10 = encounter "Mankey" 16
    }

route6table : EncounterTable
route6table =
    { name = "Route 6"
    , rate = 15
    , slot1 = encounter "Oddish" 13
    , slot2 = encounter "Pidgey" 13
    , slot3 = encounter "Pidgey" 15
    , slot4 = encounter "Mankey" 10
    , slot5 = encounter "Mankey" 12
    , slot6 = encounter "Oddish" 15
    , slot7 = encounter "Oddish" 16
    , slot8 = encounter "Pidgey" 16
    , slot9 = encounter "Mankey" 14
    , slot10 = encounter "Mankey" 16
    }

route7table : EncounterTable
route7table =
    { name = "Route 7"
    , rate = 15
    , slot1 = encounter "Pidgey" 19
    , slot2 = encounter "Oddish" 19
    , slot3 = encounter "Mankey" 17
    , slot4 = encounter "Oddish" 22
    , slot5 = encounter "Pidgey" 22
    , slot6 = encounter "Mankey" 18
    , slot7 = encounter "Growlithe" 18
    , slot8 = encounter "Growlithe" 20
    , slot9 = encounter "Mankey" 19
    , slot10 = encounter "Mankey" 20
    }

route8table : EncounterTable
route8table =
    { name = "Route 8"
    , rate = 15
    , slot1 = encounter "Pidgey" 18
    , slot2 = encounter "Mankey" 18
    , slot3 = encounter "Ekans" 17
    , slot4 = encounter "Growlithe" 16
    , slot5 = encounter "Pidgey" 20
    , slot6 = encounter "Mankey" 20
    , slot7 = encounter "Ekans" 19
    , slot8 = encounter "Growlithe" 17
    , slot9 = encounter "Growlithe" 15
    , slot10 = encounter "Growlithe" 18
    }

route9table : EncounterTable
route9table =
    { name = "Route 9"
    , rate = 15
    , slot1 = encounter "Rattata" 16
    , slot2 = encounter "Spearow" 16
    , slot3 = encounter "Rattata" 14
    , slot4 = encounter "Ekans" 11
    , slot5 = encounter "Spearow" 13
    , slot6 = encounter "Ekans" 15
    , slot7 = encounter "Rattata" 17
    , slot8 = encounter "Spearow" 17
    , slot9 = encounter "Ekans" 13
    , slot10 = encounter "Ekans" 17
    }

route10table : EncounterTable
route10table =
    { name = "Route 10"
    , rate = 15
    , slot1 = encounter "Voltorb" 16
    , slot2 = encounter "Spearow" 16
    , slot3 = encounter "Voltorb" 14
    , slot4 = encounter "Ekans" 11
    , slot5 = encounter "Spearow" 13
    , slot6 = encounter "Ekans" 15
    , slot7 = encounter "Voltorb" 17
    , slot8 = encounter "Spearow" 17
    , slot9 = encounter "Ekans" 13
    , slot10 = encounter "Ekans" 17
    }

route11table : EncounterTable
route11table =
    { name = "Route 11"
    , rate = 15
    , slot1 = encounter "Ekans" 14
    , slot2 = encounter "Spearow" 15
    , slot3 = encounter "Ekans" 12
    , slot4 = encounter "Drowzee" 9
    , slot5 = encounter "Spearow" 13
    , slot6 = encounter "Drowzee" 13
    , slot7 = encounter "Ekans" 15
    , slot8 = encounter "Spearow" 17
    , slot9 = encounter "Drowzee" 11
    , slot10 = encounter "Drowzee" 15
    }

route12table : EncounterTable
route12table =
    { name = "Route 12"
    , rate = 15
    , slot1 = encounter "Oddish" 24
    , slot2 = encounter "Pidgey" 25
    , slot3 = encounter "Pidgey" 23
    , slot4 = encounter "Venonat" 24
    , slot5 = encounter "Oddish" 22
    , slot6 = encounter "Venonat" 26
    , slot7 = encounter "Oddish" 26
    , slot8 = encounter "Pidgey" 27
    , slot9 = encounter "Gloom" 28
    , slot10 = encounter "Gloom" 30
    }

route13table : EncounterTable
route13table =
    { name = "Route 13"
    , rate = 20
    , slot1 = encounter "Oddish" 24
    , slot2 = encounter "Pidgey" 25
    , slot3 = encounter "Pidgey" 27
    , slot4 = encounter "Venonat" 24
    , slot5 = encounter "Oddish" 22
    , slot6 = encounter "Venonat" 26
    , slot7 = encounter "Oddish" 26
    , slot8 = encounter "Ditto" 25
    , slot9 = encounter "Gloom" 28
    , slot10 = encounter "Gloom" 30
    }

route14table : EncounterTable
route14table =
    { name = "Route 14"
    , rate = 15
    , slot1 = encounter "Oddish" 24
    , slot2 = encounter "Pidgey" 26
    , slot3 = encounter "Ditto" 23
    , slot4 = encounter "Venonat" 24
    , slot5 = encounter "Oddish" 22
    , slot6 = encounter "Venonat" 26
    , slot7 = encounter "Oddish" 26
    , slot8 = encounter "Gloom" 30
    , slot9 = encounter "Pidgeotto" 28
    , slot10 = encounter "Pidgeotto" 30
    }

route15table : EncounterTable
route15table =
    { name = "Route 15"
    , rate = 15
    , slot1 = encounter "Oddish" 24
    , slot2 = encounter "Ditto" 26
    , slot3 = encounter "Pidgey" 23
    , slot4 = encounter "Venonat" 26
    , slot5 = encounter "Oddish" 22
    , slot6 = encounter "Venonat" 28
    , slot7 = encounter "Oddish" 26
    , slot8 = encounter "Gloom" 30
    , slot9 = encounter "Pidgeotto" 28
    , slot10 = encounter "Pidgeotto" 30
    }

route16table : EncounterTable
route16table =
    { name = "Route 16"
    , rate = 25
    , slot1 = encounter "Spearow" 20
    , slot2 = encounter "Spearow" 22
    , slot3 = encounter "Rattata" 18
    , slot4 = encounter "Doduo" 20
    , slot5 = encounter "Rattata" 20
    , slot6 = encounter "Doduo" 18
    , slot7 = encounter "Doduo" 22
    , slot8 = encounter "Rattata" 22
    , slot9 = encounter "Raticate" 23
    , slot10 = encounter "Raticate" 25
    }

route17table : EncounterTable
route17table =
    { name = "Route 17"
    , rate = 25
    , slot1 = encounter "Spearow" 20
    , slot2 = encounter "Spearow" 22
    , slot3 = encounter "Raticate" 25
    , slot4 = encounter "Doduo" 24
    , slot5 = encounter "Raticate" 27
    , slot6 = encounter "Doduo" 26
    , slot7 = encounter "Doduo" 28
    , slot8 = encounter "Raticate" 29
    , slot9 = encounter "Fearow" 25
    , slot10 = encounter "Fearow" 27
    }

route18table : EncounterTable
route18table =
    { name = "Route 18"
    , rate = 25
    , slot1 = encounter "Spearow" 20
    , slot2 = encounter "Spearow" 22
    , slot3 = encounter "Raticate" 25
    , slot4 = encounter "Doduo" 24
    , slot5 = encounter "Fearow" 25
    , slot6 = encounter "Doduo" 26
    , slot7 = encounter "Doduo" 28
    , slot8 = encounter "Raticate" 29
    , slot9 = encounter "Fearow" 27
    , slot10 = encounter "Fearow" 29
    }

seaRoute19waterTable : EncounterTable
seaRoute19waterTable =
    { name = "Sea Route 19 (water)"
    , rate = 5
    , slot1 = encounter "Tentacool" 5
    , slot2 = encounter "Tentacool" 10
    , slot3 = encounter "Tentacool" 15
    , slot4 = encounter "Tentacool" 5
    , slot5 = encounter "Tentacool" 10
    , slot6 = encounter "Tentacool" 15
    , slot7 = encounter "Tentacool" 20
    , slot8 = encounter "Tentacool" 30
    , slot9 = encounter "Tentacool" 35
    , slot10 = encounter "Tentacool" 40
    }

seaRoute20waterTable : EncounterTable
seaRoute20waterTable =
    { name = "Sea Route 20 (water)"
    , rate = 5
    , slot1 = encounter "Tentacool" 5
    , slot2 = encounter "Tentacool" 10
    , slot3 = encounter "Tentacool" 15
    , slot4 = encounter "Tentacool" 5
    , slot5 = encounter "Tentacool" 10
    , slot6 = encounter "Tentacool" 15
    , slot7 = encounter "Tentacool" 20
    , slot8 = encounter "Tentacool" 30
    , slot9 = encounter "Tentacool" 35
    , slot10 = encounter "Tentacool" 40
    }

seaRoute21table : EncounterTable
seaRoute21table =
    { name = "Sea Route 21"
    , rate = 25
    , slot1 = encounter "Rattata" 21
    , slot2 = encounter "Pidgey" 23
    , slot3 = encounter "Raticate" 30
    , slot4 = encounter "Rattata" 23
    , slot5 = encounter "Pidgey" 21
    , slot6 = encounter "Pidgeotto" 30
    , slot7 = encounter "Pidgeotto" 32
    , slot8 = encounter "Tangela" 28
    , slot9 = encounter "Tangela" 30
    , slot10 = encounter "Tangela" 32
    }

seaRoute21waterTable : EncounterTable
seaRoute21waterTable =
    { name = "Sea Route 21 (water)"
    , rate = 5
    , slot1 = encounter "Tentacool" 5
    , slot2 = encounter "Tentacool" 10
    , slot3 = encounter "Tentacool" 15
    , slot4 = encounter "Tentacool" 5
    , slot5 = encounter "Tentacool" 10
    , slot6 = encounter "Tentacool" 15
    , slot7 = encounter "Tentacool" 20
    , slot8 = encounter "Tentacool" 30
    , slot9 = encounter "Tentacool" 35
    , slot10 = encounter "Tentacool" 40
    }

route22table : EncounterTable
route22table =
    { name = "Route 22"
    , rate = 25
    , slot1 = encounter "Rattata" 3
    , slot2 = encounter "Nidoran M" 3
    , slot3 = encounter "Rattata" 4
    , slot4 = encounter "Nidoran M" 4
    , slot5 = encounter "Rattata" 2
    , slot6 = encounter "Nidoran M" 2
    , slot7 = encounter "Spearow" 3
    , slot8 = encounter "Spearow" 5
    , slot9 = encounter "Nidoran F" 3
    , slot10 = encounter "Nidoran F" 4
    }

route23table : EncounterTable
route23table =
    { name = "Route 23"
    , rate = 10
    , slot1 = encounter "Ekans" 26
    , slot2 = encounter "Ditto" 33
    , slot3 = encounter "Spearow" 26
    , slot4 = encounter "Fearow" 38
    , slot5 = encounter "Ditto" 38
    , slot6 = encounter "Fearow" 38
    , slot7 = encounter "Arbok" 41
    , slot8 = encounter "Ditto" 43
    , slot9 = encounter "Fearow" 41
    , slot10 = encounter "Fearow" 43
    }

route24table : EncounterTable
route24table =
    { name = "Route 24"
    , rate = 25
    , slot1 = encounter "Weedle" 7
    , slot2 = encounter "Kakuna" 8
    , slot3 = encounter "Pidgey" 12
    , slot4 = encounter "Oddish" 12
    , slot5 = encounter "Oddish" 13
    , slot6 = encounter "Abra" 10
    , slot7 = encounter "Oddish" 14
    , slot8 = encounter "Pidgey" 13
    , slot9 = encounter "Abra" 8
    , slot10 = encounter "Abra" 12
    }

route25table : EncounterTable
route25table =
    { name = "Route 25"
    , rate = 15
    , slot1 = encounter "Weedle" 8
    , slot2 = encounter "Kakuna" 9
    , slot3 = encounter "Pidgey" 13
    , slot4 = encounter "Oddish" 12
    , slot5 = encounter "Oddish" 13
    , slot6 = encounter "Abra" 12
    , slot7 = encounter "Oddish" 14
    , slot8 = encounter "Abra" 10
    , slot9 = encounter "Metapod" 7
    , slot10 = encounter "Caterpie" 8
    }

viridianForestTable : EncounterTable
viridianForestTable =
    { name = "Viridian Forest"
    , rate = 8
    , slot1 = encounter "Weedle" 4
    , slot2 = encounter "Kakuna" 5
    , slot3 = encounter "Weedle" 3
    , slot4 = encounter "Weedle" 5
    , slot5 = encounter "Kakuna" 4
    , slot6 = encounter "Kakuna" 6
    , slot7 = encounter "Metapod" 4
    , slot8 = encounter "Caterpie" 3
    , slot9 = encounter "Pikachu" 3
    , slot10 = encounter "Pikachu" 5
    }

mtMoonTable1 : EncounterTable
mtMoonTable1 =
    { name = "Mt. Moon (1F)"
    , rate = 10
    , slot1 = encounter "Zubat" 8
    , slot2 = encounter "Zubat" 7
    , slot3 = encounter "Zubat" 9
    , slot4 = encounter "Geodude" 8
    , slot5 = encounter "Zubat" 6
    , slot6 = encounter "Zubat" 10
    , slot7 = encounter "Geodude" 10
    , slot8 = encounter "Paras" 8
    , slot9 = encounter "Zubat" 11
    , slot10 = encounter "Clefairy" 8
    }

mtMoonTable2 : EncounterTable
mtMoonTable2 =
    { name = "Mt. Moon (B1F)"
    , rate = 10
    , slot1 = encounter "Zubat" 8
    , slot2 = encounter "Zubat" 7
    , slot3 = encounter "Geodude" 7
    , slot4 = encounter "Geodude" 8
    , slot5 = encounter "Zubat" 9
    , slot6 = encounter "Paras" 10
    , slot7 = encounter "Zubat" 10
    , slot8 = encounter "Zubat" 11
    , slot9 = encounter "Clefairy" 9
    , slot10 = encounter "Geodude" 9
    }

mtMoonTable3 : EncounterTable
mtMoonTable3 =
    { name = "Mt. Moon (B2F)"
    , rate = 10
    , slot1 = encounter "Zubat" 9
    , slot2 = encounter "Geodude" 9
    , slot3 = encounter "Zubat" 10
    , slot4 = encounter "Geodude" 10
    , slot5 = encounter "Zubat" 11
    , slot6 = encounter "Paras" 10
    , slot7 = encounter "Paras" 12
    , slot8 = encounter "Clefairy" 10
    , slot9 = encounter "Zubat" 12
    , slot10 = encounter "Clefairy" 12
    }

rockTunnelTable1 : EncounterTable
rockTunnelTable1 =
    { name = "Rock Tunnel (1F)"
    , rate = 15
    , slot1 = encounter "Zubat" 16
    , slot2 = encounter "Zubat" 17
    , slot3 = encounter "Geodude" 17
    , slot4 = encounter "Machop" 15
    , slot5 = encounter "Geodude" 16
    , slot6 = encounter "Zubat" 18
    , slot7 = encounter "Zubat" 15
    , slot8 = encounter "Machop" 17
    , slot9 = encounter "Onix" 13
    , slot10 = encounter "Onix" 15
    }

rockTunnelTable2 : EncounterTable
rockTunnelTable2 =
    { name = "Rock Tunnel (B1F)"
    , rate = 15
    , slot1 = encounter "Zubat" 16
    , slot2 = encounter "Zubat" 17
    , slot3 = encounter "Geodude" 17
    , slot4 = encounter "Machop" 15
    , slot5 = encounter "Geodude" 16
    , slot6 = encounter "Zubat" 18
    , slot7 = encounter "Machop" 17
    , slot8 = encounter "Onix" 17
    , slot9 = encounter "Onix" 13
    , slot10 = encounter "Geodude" 18
    }

powerPlantTable : EncounterTable
powerPlantTable =
    { name = "Power Plant"
    , rate = 10
    , slot1 = encounter "Voltorb" 21
    , slot2 = encounter "Magnemite" 21
    , slot3 = encounter "Pikachu" 20
    , slot4 = encounter "Pikachu" 24
    , slot5 = encounter "Magnemite" 23
    , slot6 = encounter "Voltorb" 23
    , slot7 = encounter "Magneton" 32
    , slot8 = encounter "Magneton" 35
    , slot9 = encounter "Electabuzz" 33
    , slot10 = encounter "Electabuzz" 36
    }

victoryRoadTable1 : EncounterTable
victoryRoadTable1 =
    { name = "Victory Road (1F)"
    , rate = 15
    , slot1 = encounter "Machop" 24
    , slot2 = encounter "Geodude" 26
    , slot3 = encounter "Zubat" 22
    , slot4 = encounter "Onix" 36
    , slot5 = encounter "Onix" 39
    , slot6 = encounter "Onix" 42
    , slot7 = encounter "Graveler" 41
    , slot8 = encounter "Golbat" 41
    , slot9 = encounter "Machoke" 42
    , slot10 = encounter "Marowak" 43
    }

victoryRoadTable2 : EncounterTable
victoryRoadTable2 =
    { name = "VictoryRoad (2F)"
    , rate = 10
    , slot1 = encounter "Machop" 22
    , slot2 = encounter "Geodude" 24
    , slot3 = encounter "Zubat" 26
    , slot4 = encounter "Onix" 36
    , slot5 = encounter "Onix" 39
    , slot6 = encounter "Onix" 42
    , slot7 = encounter "Machoke" 41
    , slot8 = encounter "Golbat" 40
    , slot9 = encounter "Marowak" 40
    , slot10 = encounter "Graveler" 43
    }

victoryRoadTable3 : EncounterTable
victoryRoadTable3 =
    { name = "Victory Road (3F)"
    , rate = 15
    , slot1 = encounter "Machop" 24
    , slot2 = encounter "Geodude" 26
    , slot3 = encounter "Zubat" 22
    , slot4 = encounter "Onix" 42
    , slot5 = encounter "Venomoth" 40
    , slot6 = encounter "Onix" 45
    , slot7 = encounter "Graveler" 43
    , slot8 = encounter "Golbat" 41
    , slot9 = encounter "Machoke" 42
    , slot10 = encounter "Machoke" 45
    }

pokemonTowerTable1 : EncounterTable
pokemonTowerTable1 =
    { name = "Pokemon Tower (1F)"
    , rate = 10
    , slot1 = encounter "Gastly" 20
    , slot2 = encounter "Gastly" 21
    , slot3 = encounter "Gastly" 22
    , slot4 = encounter "Gastly" 23
    , slot5 = encounter "Gastly" 19
    , slot6 = encounter "Gastly" 18
    , slot7 = encounter "Gastly" 24
    , slot8 = encounter "Cubone" 20
    , slot9 = encounter "Cubone" 22
    , slot10 = encounter "Haunter" 25
    }

pokemonTowerTable2 : EncounterTable
pokemonTowerTable2 =
    { name = "Pokemon Tower (2F)"
    , rate = 10
    , slot1 = encounter "Gastly" 20
    , slot2 = encounter "Gastly" 21
    , slot3 = encounter "Gastly" 22
    , slot4 = encounter "Gastly" 23
    , slot5 = encounter "Gastly" 19
    , slot6 = encounter "Gastly" 18
    , slot7 = encounter "Haunter" 25
    , slot8 = encounter "Cubone" 20
    , slot9 = encounter "Cubone" 22
    , slot10 = encounter "Gastly" 24
    }

pokemonTowerTable3 : EncounterTable
pokemonTowerTable3 =
    { name = "Pokemon Tower (3F)"
    , rate = 10
    , slot1 = encounter "Gastly" 20
    , slot2 = encounter "Gastly" 21
    , slot3 = encounter "Gastly" 22
    , slot4 = encounter "Gastly" 23
    , slot5 = encounter "Gastly" 19
    , slot6 = encounter "Gastly" 18
    , slot7 = encounter "Haunter" 25
    , slot8 = encounter "Cubone" 20
    , slot9 = encounter "Cubone" 22
    , slot10 = encounter "Gastly" 24
    }

pokemonTowerTable4 : EncounterTable
pokemonTowerTable4 =
    { name = "Pokemon Tower (4F)"
    , rate = 15
    , slot1 = encounter "Gastly" 21
    , slot2 = encounter "Gastly" 22
    , slot3 = encounter "Gastly" 23
    , slot4 = encounter "Gastly" 24
    , slot5 = encounter "Gastly" 20
    , slot6 = encounter "Gastly" 19
    , slot7 = encounter "Haunter" 26
    , slot8 = encounter "Cubone" 22
    , slot9 = encounter "Cubone" 24
    , slot10 = encounter "Haunter" 28
    }

pokemonTowerTable5 : EncounterTable
pokemonTowerTable5 =
    { name = "Pokemon Tower (5F)"
    , rate = 15
    , slot1 = encounter "Gastly" 21
    , slot2 = encounter "Gastly" 22
    , slot3 = encounter "Gastly" 23
    , slot4 = encounter "Gastly" 24
    , slot5 = encounter "Gastly" 20
    , slot6 = encounter "Haunter" 28
    , slot7 = encounter "Cubone" 22
    , slot8 = encounter "Cubone" 24
    , slot9 = encounter "Haunter" 28
    , slot10 = encounter "Haunter" 30
    }

seafoamIslandsTable1 : EncounterTable
seafoamIslandsTable1 =
    { name = "Seafoam Islands (1F)"
    , rate = 15
    , slot1 = encounter "Seel" 30
    , slot2 = encounter "Slowpoke" 30
    , slot3 = encounter "Shellder" 30
    , slot4 = encounter "Horsea" 30
    , slot5 = encounter "Horsea" 28
    , slot6 = encounter "Zubat" 21
    , slot7 = encounter "Golbat" 29
    , slot8 = encounter "Psyduck" 28
    , slot9 = encounter "Shellder" 28
    , slot10 = encounter "Golduck" 38
    }

seafoamIslandsTable2 : EncounterTable
seafoamIslandsTable2 =
    { name = "Seafoam Islands (B1F)"
    , rate = 10
    , slot1 = encounter "Staryu" 30
    , slot2 = encounter "Horsea" 30
    , slot3 = encounter "Shellder" 32
    , slot4 = encounter "Horsea" 32
    , slot5 = encounter "Slowpoke" 28
    , slot6 = encounter "Seel" 30
    , slot7 = encounter "Slowpoke" 30
    , slot8 = encounter "Seel" 28
    , slot9 = encounter "Dewgong" 38
    , slot10 = encounter "Seadra" 37
    }

seafoamIslandsTable3 : EncounterTable
seafoamIslandsTable3 =
    { name = "Seafoam Islands (B2F)"
    , rate = 10
    , slot1 = encounter "Seel" 30
    , slot2 = encounter "Slowpoke" 30
    , slot3 = encounter "Seel" 32
    , slot4 = encounter "Slowpoke" 32
    , slot5 = encounter "Horsea" 28
    , slot6 = encounter "Staryu" 30
    , slot7 = encounter "Horsea" 30
    , slot8 = encounter "Shellder" 28
    , slot9 = encounter "Golbat" 30
    , slot10 = encounter "Slowbro" 37
    }

seafoamIslandsTable4 : EncounterTable
seafoamIslandsTable4 =
    { name = "Seafoam Islands (B3F)"
    , rate = 10
    , slot1 = encounter "Slowpoke" 31
    , slot2 = encounter "Seel" 31
    , slot3 = encounter "Slowpoke" 33
    , slot4 = encounter "Seel" 33
    , slot5 = encounter "Horsea" 29
    , slot6 = encounter "Shellder" 31
    , slot7 = encounter "Horsea" 31
    , slot8 = encounter "Shellder" 29
    , slot9 = encounter "Seadra" 39
    , slot10 = encounter "Dewgong" 37
    }

seafoamIslandsTable5 : EncounterTable
seafoamIslandsTable5 =
    { name = "Seafoam Islands (B4F)"
    , rate = 10
    , slot1 = encounter "Horsea" 31
    , slot2 = encounter "Shellder" 31
    , slot3 = encounter "Horsea" 33
    , slot4 = encounter "Shellder" 33
    , slot5 = encounter "Slowpoke" 29
    , slot6 = encounter "Seel" 31
    , slot7 = encounter "Slowpoke" 31
    , slot8 = encounter "Seel" 29
    , slot9 = encounter "Slowbro" 39
    , slot10 = encounter "Golbat" 32
    }

pokemonMansionTable1 : EncounterTable
pokemonMansionTable1 =
    { name = "Pokemon Mansion (1F)"
    , rate = 10
    , slot1 = encounter "Koffing" 32
    , slot2 = encounter "Koffing" 30
    , slot3 = encounter "Ponyta" 34
    , slot4 = encounter "Ponyta" 30
    , slot5 = encounter "Growlithe" 34
    , slot6 = encounter "Ponyta" 32
    , slot7 = encounter "Grimer" 30
    , slot8 = encounter "Ponyta" 28
    , slot9 = encounter "Weezing" 37
    , slot10 = encounter "Muk" 39
    }

pokemonMansionTable2 : EncounterTable
pokemonMansionTable2 =
    { name = "Pokemon Mansion (2F)"
    , rate = 10
    , slot1 = encounter "Growlithe" 32
    , slot2 = encounter "Koffing" 34
    , slot3 = encounter "Koffing" 34
    , slot4 = encounter "Ponyta" 30
    , slot5 = encounter "Koffing" 30
    , slot6 = encounter "Ponyta" 32
    , slot7 = encounter "Grimer" 30
    , slot8 = encounter "Ponyta" 28
    , slot9 = encounter "Weezing" 39
    , slot10 = encounter "Muk" 37
    }

pokemonMansionTable3 : EncounterTable
pokemonMansionTable3 =
    { name = "Pokemon Mansion (3F)"
    , rate = 10
    , slot1 = encounter "Koffing" 31
    , slot2 = encounter "Growlithe" 33
    , slot3 = encounter "Koffing" 35
    , slot4 = encounter "Ponyta" 32
    , slot5 = encounter "Ponyta" 34
    , slot6 = encounter "Weezing" 40
    , slot7 = encounter "Grimer" 34
    , slot8 = encounter "Weezing" 38
    , slot9 = encounter "Ponyta" 36
    , slot10 = encounter "Muk" 42
    }

pokemonMansionTable4 : EncounterTable
pokemonMansionTable4 =
    { name = "Pokemon Mansion (B1F)"
    , rate = 10
    , slot1 = encounter "Koffing" 33
    , slot2 = encounter "Koffing" 31
    , slot3 = encounter "Growlithe" 35
    , slot4 = encounter "Ponyta" 32
    , slot5 = encounter "Koffing" 31
    , slot6 = encounter "Weezing" 40
    , slot7 = encounter "Ponyta" 34
    , slot8 = encounter "Grimer" 35
    , slot9 = encounter "Weezing" 42
    , slot10 = encounter "Muk" 42
    }

diglettsCaveTable : EncounterTable
diglettsCaveTable =
    { name = "Diglett's Cave"
    , rate = 20
    , slot1 = encounter "Diglett" 18
    , slot2 = encounter "Diglett" 19
    , slot3 = encounter "Diglett" 17
    , slot4 = encounter "Diglett" 20
    , slot5 = encounter "Diglett" 16
    , slot6 = encounter "Diglett" 15
    , slot7 = encounter "Diglett" 21
    , slot8 = encounter "Diglett" 22
    , slot9 = encounter "Dugtrio" 29
    , slot10 = encounter "Dugtrio" 31
    }

safariZoneTable1 : EncounterTable
safariZoneTable1 =
    { name = "Safari Zone 1"
    , rate = 30
    , slot1 = encounter "Nidoran M" 22
    , slot2 = encounter "Rhyhorn" 25
    , slot3 = encounter "Venonat" 22
    , slot4 = encounter "Exeggcute" 24
    , slot5 = encounter "Nidorino" 31
    , slot6 = encounter "Exeggcute" 25
    , slot7 = encounter "Nidorina" 31
    , slot8 = encounter "Parasect" 30
    , slot9 = encounter "Scyther" 23
    , slot10 = encounter "Chansey" 23
    }

safariZoneTable2 : EncounterTable
safariZoneTable2 =
    { name = "Safari Zone 2"
    , rate = 30
    , slot1 = encounter "Nidoran M" 24
    , slot2 = encounter "Doduo" 26
    , slot3 = encounter "Paras" 22
    , slot4 = encounter "Exeggcute" 25
    , slot5 = encounter "Nidorino" 33
    , slot6 = encounter "Exeggcute" 23
    , slot7 = encounter "Nidoran F" 24
    , slot8 = encounter "Parasect" 25
    , slot9 = encounter "Kangaskhan" 25
    , slot10 = encounter "Scyther" 28
    }

safariZoneTable3 : EncounterTable
safariZoneTable3 =
    { name = "Safari Zone 3"
    , rate = 30
    , slot1 = encounter "Nidoran M" 22
    , slot2 = encounter "Rhyhorn" 26
    , slot3 = encounter "Paras" 23
    , slot4 = encounter "Exeggcute" 25
    , slot5 = encounter "Nidorino" 30
    , slot6 = encounter "Exeggcute" 27
    , slot7 = encounter "Nidorina" 30
    , slot8 = encounter "Venomoth" 32
    , slot9 = encounter "Chansey" 26
    , slot10 = encounter "Tauros" 28
    }

safariZoneTable4 : EncounterTable
safariZoneTable4 =
    { name = "Safari Zone 4"
    , rate = 30
    , slot1 = encounter "Nidoran M" 25
    , slot2 = encounter "Doduo" 26
    , slot3 = encounter "Venonat" 23
    , slot4 = encounter "Exeggcute" 24
    , slot5 = encounter "Nidorino" 33
    , slot6 = encounter "Exeggcute" 26
    , slot7 = encounter "Nidoran F" 25
    , slot8 = encounter "Venomoth" 31
    , slot9 = encounter "Tauros" 26
    , slot10 = encounter "Kangaskhan" 28
    }

ceruleanCaveTable1 : EncounterTable
ceruleanCaveTable1 =
    { name = "Cerulean Cave (1F)"
    , rate = 10
    , slot1 = encounter "Golbat" 46
    , slot2 = encounter "Hypno" 46
    , slot3 = encounter "Magneton" 46
    , slot4 = encounter "Dodrio" 49
    , slot5 = encounter "Venomoth" 49
    , slot6 = encounter "Arbok" 52
    , slot7 = encounter "Kadabra" 49
    , slot8 = encounter "Parasect" 52
    , slot9 = encounter "Raichu" 53
    , slot10 = encounter "Ditto" 53
    }

ceruleanCaveTable2 : EncounterTable
ceruleanCaveTable2 =
    { name = "Cerulean Cave (2F)"
    , rate = 15
    , slot1 = encounter "Dodrio" 51
    , slot2 = encounter "Venomoth" 51
    , slot3 = encounter "Kadabra" 51
    , slot4 = encounter "Rhydon" 52
    , slot5 = encounter "Marowak" 52
    , slot6 = encounter "Electrode" 52
    , slot7 = encounter "Chansey" 56
    , slot8 = encounter "Wigglytuff" 54
    , slot9 = encounter "Ditto" 55
    , slot10 = encounter "Ditto" 60
    }

ceruleanCaveTable3 : EncounterTable
ceruleanCaveTable3 =
    { name = "Cerulean Cave (B1F)"
    , rate = 25
    , slot1 = encounter "Rhydon" 55
    , slot2 = encounter "Marowak" 55
    , slot3 = encounter "Electrode" 55
    , slot4 = encounter "Chansey" 64
    , slot5 = encounter "Parasect" 64
    , slot6 = encounter "Raichu" 64
    , slot7 = encounter "Arbok" 57
    , slot8 = encounter "Ditto" 65
    , slot9 = encounter "Ditto" 63
    , slot10 = encounter "Ditto" 67
    }
