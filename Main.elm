import Color exposing (..)
import Dict
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

import DApprox exposing (..)
import DSum exposing (..)
import Dist
import Encounters exposing (..)
import Graph exposing (..)
import Interface exposing (..)
import RNG exposing (..)
import Pokemon exposing (..)
import Query exposing (..)
import Strategy exposing (..)
import Worker exposing (..)

thresholdBox : Signal.Mailbox Content
thresholdBox = Signal.mailbox noContent

thresholdSignal : Signal Float
thresholdSignal = Signal.map (Maybe.withDefault 0.25 << Result.toMaybe << String.toFloat << .string) thresholdBox.signal

thresholdInput : Signal Element
thresholdInput = field defaultStyle (Signal.message thresholdBox.address) "Threshold (default 0.25)" <~ thresholdBox.signal

calculateBox : Signal.Mailbox ()
calculateBox = Signal.mailbox ()

encounteredSlots : Signal (List Int)
encounteredSlots = Signal.constant [3]

calculateButton : Element
calculateButton = button (Signal.message calculateBox.address ()) "Calculate"

contentString : Content -> String
contentString content = content.string

iterate : Int -> (a -> a) -> a -> a
iterate n f x = trampoline (iterate' n f x)

iterate' : Int -> (a -> a) -> a -> Trampoline a
iterate' n f x = if
    | n == 0 -> Trampoline.Done x
    | otherwise -> Continue (\() -> iterate' (n-1) f (f x))

successProbability : Int -> List Int -> DSumState -> Float
successProbability rate slots state =
    state
    |> dsumDist
    |> Dist.collapseMap (dsumSlotDist rate)
    |> Dist.probability (\s -> List.member s slots)

approxProbability : Int -> List Int -> DApproxState -> Float
approxProbability rate slots state =
    state
    |> dapproxDist
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
dsumPath n carry state = toPath << List.map toFloat << snd <| dsums n carry state

sampleEncounterDSums : RNGState -> List Int
sampleEncounterDSums state =
    let (state', sums) = dsums 594 1 state
        (state'', sums') = dsums 44 0 state'
        (_, sums'') = dsums 1000 0 state''
    in List.concat [sums, sums', sums'']

dsums : Int -> Int -> RNGState -> (RNGState, List Int)
dsums n carry state = if
    | n == 0 -> (state, [])
    | otherwise ->
        let (finalState, sums) = dsums (n-1) carry (rngStep carry state)
        in (finalState, getDSum state :: sums)

combine : List (Signal a) -> Signal (List a)
combine = List.foldr (Signal.map2 (::)) (Signal.constant [])

buildStrategy : Float -> List Float -> Strategy
buildStrategy threshold = simplify 15 << frameStrategy << List.map (\x -> x >= threshold)

exampleQuery : Query
exampleQuery =
    { duration = 1000
    , offset = framesBeforeMove
    , successFunc = Dist.probability (\x -> List.member x [2, 4]) << dsumSlotDist 25 
    , initialSteps =
        [ QCondition (Dist.probability (\x -> x == 3) << dsumSlotDist 25)
        , QAdvance 595 insideSlopeDist
        ]
    }

queryWorker : Worker QueryWorkerState (List Float)
queryWorker = createQueryWorker (Signal.sampleOn calculateBox.signal querySignal)

queryProbabilitiesSignal : Signal (List Float)
queryProbabilitiesSignal = Signal.map (\state ->
    case snd state of
        Working (_, _, _, acc) -> List.reverse acc
        Worker.Done probs -> probs
        Unstarted -> []
    ) queryWorker.state

queryGraph : Signal Graph
queryGraph = graph (Just (0, 1000)) (Just (0, 1)) << (\x -> [x]) << toPath <~ queryProbabilitiesSignal

strategy : Signal Strategy
strategy = buildStrategy <~ thresholdSignal ~ (Signal.map (Maybe.withDefault []) queryWorker.signal)

strategy2 : Signal Strategy
strategy2 = roundStrategy 17 <~ strategy

stepStrategy : Signal (List (Int, Bool))
stepStrategy = List.map (\s -> (s.frames // 17, s.inGrass)) <~ strategy2

interfaceStateBox : Signal.Mailbox InterfaceState
interfaceStateBox = Signal.mailbox defaultInterfaceState

interface : Signal Element
interface = queryInterface (Signal.message interfaceStateBox.address) <~ interfaceStateBox.signal

querySignal : Signal Query
querySignal = buildQuery <~ interfaceStateBox.signal

main : Signal Element
main = flow down <~ combine
    [ interface
    , Signal.constant calculateButton
    , thresholdInput
    , drawGraph 700 400 <~ queryGraph
    , Signal.map show strategy
    , Signal.map show strategy2
    , Signal.map show stepStrategy
    ]
