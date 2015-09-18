module DApprox where

import Dict
import List

import Dist exposing (Dist)
import Encounters exposing (slotFromRand)

type alias DApproxState =
    { muDist : Dist Float
    , thetaDist : Dist Float
    }

initialDApproxState : DApproxState
initialDApproxState =
    { muDist = Dist.uniform [0..255]
    , thetaDist = [0..15]
        |> List.map (\x -> x * 2 * pi / 16)
        |> Dist.uniform
    }

dsumApproximation : Float -> Float -> Int
dsumApproximation mu theta = (round (mu + 5.45 + 2.47*sin(theta))) % 256

outsideHighSlope = -81664 / 131072
outsideLowSlope = -89856 / 131072
insideHighSlope = 48896 / 131072
insideLowSlope = 40704 / 131072
dTheta = 3 * pi / 1024

dapproxDist : DApproxState -> Dist Int
dapproxDist s = Dist.lift2 dsumApproximation s.muDist s.thetaDist

outsideSlopeDist : Dist Float
outsideSlopeDist = Dict.fromList
    [ (outsideLowSlope, 3/4)
    , (outsideHighSlope, 1/4)
    ]

insideSlopeDist : Dist Float
insideSlopeDist = Dict.fromList
    [ (insideLowSlope, 3/4)
    , (insideHighSlope, 1/4)
    ]

floatMod : Float -> Float -> Float
floatMod x m = x - m * (toFloat (floor (x / m)))

advanceDApprox : Dist Float -> Int -> DApproxState -> DApproxState
advanceDApprox slopeDist n s =
    { muDist = Dist.lift2 (\slope mu -> floatMod (mu + toFloat n * slope) 256) slopeDist s.muDist
    , thetaDist = Dist.map (\theta -> floatMod (theta + toFloat n * dTheta) (2 * pi)) s.thetaDist
    }

-- The first argument specifies whether the corresponding DSum should be included
-- in the output.
filterDApprox : (Int -> Bool) -> DApproxState -> DApproxState
filterDApprox f s =
    let muDist' = filterMu f s.thetaDist s.muDist
        thetaDist' = filterTheta f muDist' s.thetaDist
    in
    { muDist = muDist'
    , thetaDist = thetaDist'
    }

filterMu : (Int -> Bool) -> Dist Float -> Dist Float -> Dist Float
filterMu f thetaDist =
    Dist.condition (\mu ->
        Dist.probability f (Dist.map (dsumApproximation mu) thetaDist))

filterTheta : (Int -> Bool) -> Dist Float -> Dist Float -> Dist Float
filterTheta f muDist =
    Dist.condition (\theta ->
        Dist.probability f (Dist.map (\mu -> dsumApproximation mu theta) muDist))

conditionDApprox : (Int -> Float) -> DApproxState -> DApproxState
conditionDApprox f s =
    let muDist' = conditionMu f s.thetaDist s.muDist
        thetaDist' = conditionTheta f muDist' s.thetaDist
    in
    { muDist = muDist'
    , thetaDist = thetaDist'
    }

conditionMu : (Int -> Float) -> Dist Float -> Dist Float -> Dist Float
conditionMu f thetaDist =
    Dist.condition (\mu ->
        Dist.weightedProbability f (Dist.map (dsumApproximation mu) thetaDist))

conditionTheta : (Int -> Float) -> Dist Float -> Dist Float -> Dist Float
conditionTheta f muDist =
    Dist.condition (\theta ->
        Dist.weightedProbability f (Dist.map (\mu -> dsumApproximation mu theta) muDist))
