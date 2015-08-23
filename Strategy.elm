module Strategy where

import List

type alias StrategyStep =
    { inGrass : Bool
    , frames : Int
    }

type alias Strategy = List StrategyStep

combineSteps : Strategy -> Strategy
combineSteps l = case l of
    [] -> []
    x::[] -> x::[]
    x::y::xs -> if x.inGrass == y.inGrass
        then combineSteps ({ inGrass = x.inGrass, frames = x.frames + y.frames } :: xs)
        else x :: combineSteps (y::xs)

removeSmallSteps : Int -> Strategy -> Strategy
removeSmallSteps frameThreshold strat = case strat of
    [] -> []
    x::[] -> x::[]
    x::y::xs -> if x.frames < frameThreshold
        then removeSmallSteps frameThreshold ({ inGrass = y.inGrass, frames = x.frames + y.frames } :: xs)
        else x :: removeSmallSteps frameThreshold (y::xs)

simplify : Int -> Strategy -> Strategy
simplify frameThreshold strat = strat
    |> combineSteps
    |> removeSmallSteps frameThreshold
    |> combineSteps

frameStrategy : List Bool -> Strategy
frameStrategy l = l
    |> List.map (\b -> {inGrass = b, frames = 1})
    |> combineSteps

roundStrategy : Int -> Strategy -> Strategy
roundStrategy rounding strat =
    roundStrategy' rounding 0 strat
    |> combineSteps

roundStrategy' : Int -> Int -> Strategy -> Strategy
roundStrategy' rounding extraFrames strat =
    case strat of
        [] -> []
        x::xs ->
            let
                idealFrameCount = x.frames + extraFrames
                roundedFrameCount = rounding * round (toFloat idealFrameCount / toFloat rounding)
                extras = idealFrameCount - roundedFrameCount
            in
            { x | frames <- roundedFrameCount } :: roundStrategy' rounding extras xs
