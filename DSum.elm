module DSum where

import Dict
import List

import Dist exposing (Dist)
import RNG exposing (..)
import Encounters exposing (slotFromRand)

initialRDiv = 17

type alias DSumState = Dist ComparableRNGState

initialRNGMix : DSumState
initialRNGMix = Dict.fromList << List.concat <|
    [ List.map (\dsum -> ((initialRDiv, 4, dsum, 0), 3/1024)) [0 .. 255]
    , List.map (\dsum -> ((initialRDiv, 0, dsum, 0), 1/1024)) [0 .. 255]
    ]

filterDSum : (Int -> Bool) -> DSumState -> DSumState
filterDSum f dist = Dist.filter (f << getDSum') dist

dsumDist : DSumState -> Dist Int
dsumDist = Dist.map getDSum'

dsumStep : Int -> DSumState -> DSumState
dsumStep carry dist = Dist.map (rngStep' carry) dist

dsumSlotDist : Int -> Int -> Dist Int
dsumSlotDist rate dsum =
    [0 .. rate - 1]
    |> List.map (\r1 -> (dsum - r1) % 256)
    |> Dist.uniform
    |> Dist.map slotFromRand
