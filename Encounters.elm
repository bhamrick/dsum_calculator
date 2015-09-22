module Encounters where

import Dict
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
    [ route22table
    ]

-- Encounter tables for red
-- TODO: Add other tables
-- TODO: Include blue tables
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
