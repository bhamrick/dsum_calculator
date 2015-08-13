import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
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
import Dist
import DSum exposing (..)

dsumLow : Signal.Mailbox Content
dsumLow = Signal.mailbox noContent

dsumLowSignal : Signal Int
dsumLowSignal = Signal.sampleOn calculateBox.signal (Maybe.withDefault 0 << toMaybe << toInt << contentString <~ dsumLow.signal)

dsumLowField : Signal Element
dsumLowField = field defaultStyle (Signal.message dsumLow.address) "DSum lower bound" <~ dsumLow.signal

dsumHigh : Signal.Mailbox Content
dsumHigh = Signal.mailbox noContent

dsumHighSignal : Signal Int
dsumHighSignal = Signal.sampleOn calculateBox.signal (Maybe.withDefault 25 << toMaybe << toInt << contentString <~ dsumHigh.signal)

dsumHighField : Signal Element
dsumHighField = field defaultStyle (Signal.message dsumHigh.address) "DSum upper bound" <~ dsumHigh.signal

calculateBox : Signal.Mailbox ()
calculateBox = Signal.mailbox ()

desiredSlots : Signal (List Int)
desiredSlots = Signal.constant [2, 4]

calculateButton : Element
calculateButton = button (Signal.message calculateBox.address ()) "Calculate"

initialRNGStates : Signal (List RNGState)
initialRNGStates =
    (\hRandomAdd hRandomSub ->
        List.concatMap (\rDiv ->
            List.map (\cycle ->
                rngState rDiv cycle hRandomAdd hRandomSub
            ) [0, 4]
        ) [17]
    ) <~ Signal.constant 0 ~ Signal.constant 0

contentString : Content -> String
contentString content = content.string

iterate : Int -> (a -> a) -> a -> a
iterate n f x = trampoline (iterate' n f x)

iterate' : Int -> (a -> a) -> a -> Trampoline a
iterate' n f x = if
    | n == 0 -> Done x
    | otherwise -> Continue (\() -> iterate' (n-1) f (f x))

{-
output : Signal Element
-- output = show << List.map (iterate 1500 (rngStep 0)) <~ Signal.sampleOn calculateBox.signal initialRNGStates
output = initialRNGMix
    |> filterDSum (\x -> x >= 50 && x <= 101)
    |> iterate 1000 (dsumStep 0)
    |> dsumDist
    |> Dist.collapseMap (dsumSlotDist 25)
    |> show
    |> Signal.constant
-}

successProbability : Int -> List Int -> DSumState -> Float
successProbability rate slots state =
    state
    |> dsumDist
    |> Dist.collapseMap (dsumSlotDist rate)
    |> Dist.probability (\s -> List.member s slots)

successProbabilities : Int -> List Int -> Int -> Int -> DSumState -> List Float
successProbabilities rate slots n carry state = if n <= 0 then [] else
    successProbability rate slots state
        :: successProbabilities rate slots (n-1) carry (dsumStep carry state)

toPath : List Float -> List (Float, Float)
toPath = toPath' 0

toPath' : Float -> List Float -> List (Float, Float)
toPath' n l = case l of
    [] -> []
    x::xs -> (n, x) :: toPath' (n+1) xs

dsumPath : Int -> Int -> RNGState -> List (Float, Float)
dsumPath n carry state = List.map (\(x,y) -> (toFloat x, toFloat y)) << List.map2 (,) [0 .. (n-1)] <| dsums n carry state

dsums : Int -> Int -> RNGState -> List Int
dsums n carry state = if
    | n == 0 -> []
    | otherwise -> getDSum state :: dsums (n-1) carry (rngStep carry state)

combine : List (Signal a) -> Signal (List a)
combine = List.foldr (Signal.map2 (::)) (Signal.constant [])

dsumGraph : Signal Graph
dsumGraph = graph (Just (0, 1000)) (Just (0, 255)) << List.map (dsumPath 1000 0) <~ Signal.sampleOn calculateBox.signal initialRNGStates

buildSuccessGraph : Int -> Int -> DSumState -> Graph
buildSuccessGraph low high state =
    state
    |> filterDSum (\x -> (x - low) % 256 < (high - low) % 256)
    |> successProbabilities 25 [2, 4] 1000 0
    |> toPath
    |> (\x -> [x])
    |> graph (Just (0, 1000)) (Just (0, 1))

successGraph : Signal Graph
successGraph = buildSuccessGraph <~ dsumLowSignal ~ dsumHighSignal ~ Signal.constant initialRNGMix

main : Signal Element
main = flow down <~ combine
    [ dsumLowField
    , dsumHighField
    , Signal.constant calculateButton
    -- , output
    -- , drawGraph 700 400 <~ dsumGraph
    , drawGraph 700 400 <~ successGraph
    ]
