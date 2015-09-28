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
    , route22table
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
