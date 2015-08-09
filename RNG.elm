module RNG where

type alias RNGState =
    { rDiv : Int
    , cycle : Int
    , hRandomAdd : Int
    , hRandomSub : Int
    }

rngState : Int -> Int -> Int -> Int -> RNGState
rngState r c a s = {rDiv = r, cycle = c, hRandomAdd = a, hRandomSub = s}

rngStep : Int -> RNGState -> RNGState
rngStep carry s =
    let hRandomAdd' = s.hRandomAdd + s.rDiv + carry
        cycle' = s.cycle + 44
        carry' = if hRandomAdd' >= 256 then 1 else 0
        rDiv' = if cycle' >= 256 then ((s.rDiv + 1) % 256) else s.rDiv
        hRandomSub' = s.hRandomSub - rDiv' - carry'

        rDiv'' = (s.rDiv + ((s.cycle + 70224) // 256)) % 256
        cycle'' = (s.cycle + 70224) % 256
    in
    { rDiv = rDiv''
    , cycle = cycle''
    , hRandomAdd = hRandomAdd' % 256
    , hRandomSub = hRandomSub' % 256
    }

getDSum : RNGState -> Int
getDSum s = (s.hRandomAdd + s.hRandomSub) % 256
