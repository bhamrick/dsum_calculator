module Encounters where

import Dict
import Maybe

import Pokemon exposing (..)

type alias Encounter =
    { species : Species
    , level : Int
    }

slotWidths = [51, 51, 39, 25, 25, 25, 13, 13, 11, 3]

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
