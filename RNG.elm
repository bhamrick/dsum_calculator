module RNG where

type alias RNGState =
    { rDiv : Int
    , cycle : Int
    , hRandomAdd : Int
    , hRandomSub : Int
    }

type alias ComparableRNGState = (Int, Int, Int, Int)

toComparable : RNGState -> ComparableRNGState
toComparable state = (state.rDiv, state.cycle, state.hRandomAdd, state.hRandomSub)

fromComparable : ComparableRNGState -> RNGState
fromComparable (rDiv, cycle, hRandomAdd, hRandomSub) = rngState rDiv cycle hRandomAdd hRandomSub

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

rngStep' : Int -> ComparableRNGState -> ComparableRNGState
rngStep' carry s = s
    |> fromComparable
    |> rngStep carry
    |> toComparable

getDSum : RNGState -> Int
getDSum s = (s.hRandomAdd + s.hRandomSub) % 256

getDSum' : ComparableRNGState -> Int
getDSum' (_, _, r1, r2) = (r1 + r2) % 256
