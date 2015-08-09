import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import List
import Maybe exposing (..)
import Result exposing (toMaybe)
import Signal exposing ((<~), (~))
import String exposing (toInt)
import Text exposing (fromString)
import Trampoline exposing (..)

import RNG exposing (..)
import Graph exposing (..)

rDivInput : Signal.Mailbox Content
rDivInput = Signal.mailbox noContent

rDivField : Signal Element
rDivField = field defaultStyle (Signal.message rDivInput.address) "rDiv" <~ rDivInput.signal

rDivSignal : Signal Int
rDivSignal = withDefault 0 << toMaybe << toInt << contentString <~ rDivInput.signal

cycleInput : Signal.Mailbox Content
cycleInput = Signal.mailbox noContent

cycleField : Signal Element
cycleField = field defaultStyle (Signal.message cycleInput.address) "cycle" <~ cycleInput.signal

cycleSignal : Signal Int
cycleSignal = withDefault 0 << toMaybe << toInt << contentString <~ cycleInput.signal

hRandomAddInput : Signal.Mailbox Content
hRandomAddInput = Signal.mailbox noContent

hRandomAddField : Signal Element
hRandomAddField = field defaultStyle (Signal.message hRandomAddInput.address) "hRandomAdd" <~ hRandomAddInput.signal

hRandomAddSignal : Signal Int
hRandomAddSignal = withDefault 0 << toMaybe << toInt << contentString <~ hRandomAddInput.signal

hRandomSubInput : Signal.Mailbox Content
hRandomSubInput = Signal.mailbox noContent

hRandomSubField : Signal Element
hRandomSubField = field defaultStyle (Signal.message hRandomSubInput.address) "hRandomSub" <~ hRandomSubInput.signal

hRandomSubSignal : Signal Int
hRandomSubSignal = withDefault 0 << toMaybe << toInt << contentString <~ hRandomSubInput.signal

initialRNGState : Signal RNGState
initialRNGState = rngState <~ rDivSignal ~ cycleSignal ~ hRandomAddSignal ~ hRandomSubSignal

output : Signal Element
output = show <~ (iterate 3000 (rngStep 0) <~ initialRNGState)
-- output = show <~ (dsumPath 1500 0 <~ initialRNGState)

contentString : Content -> String
contentString content = content.string

iterate : Int -> (a -> a) -> a -> a
iterate n f x = trampoline (iterate' n f x)

iterate' : Int -> (a -> a) -> a -> Trampoline a
iterate' n f x = if
    | n == 0 -> Done x
    | otherwise -> Continue (\() -> iterate' (n-1) f (f x))

dsumPath : Int -> Int -> RNGState -> List (Float, Float)
dsumPath n carry state = List.map (\(x,y) -> (toFloat x, toFloat y)) << List.map2 (,) [0 .. (n-1)] <| dsums n carry state

dsums : Int -> Int -> RNGState -> List Int
dsums n carry state = if
    | n == 0 -> []
    | otherwise -> getDSum state :: dsums (n-1) carry (rngStep carry state)

combine : List (Signal a) -> Signal (List a)
combine = List.foldr (Signal.map2 (::)) (Signal.constant [])

dsumGraph : Signal Graph
dsumGraph = graph (Just (0, 1500)) (Just (0, 255)) << List.repeat 1 << dsumPath 1500 0 <~ initialRNGState

main : Signal Element
main = flow down <~ combine
    [ rDivField
    , cycleField
    , hRandomAddField
    , hRandomSubField
    , output
    , drawGraph 700 400 <~ dsumGraph
    ]
