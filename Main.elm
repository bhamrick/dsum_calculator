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
import RNG exposing (..)
import Pokemon exposing (..)
import Query exposing (..)
import Strategy exposing (..)
import Worker exposing (..)

encounterTable : Signal EncounterTable
encounterTable = Signal.constant route22table

slot1DesiredBox : Signal.Mailbox Bool
slot1DesiredBox = Signal.mailbox False

slot2DesiredBox : Signal.Mailbox Bool
slot2DesiredBox = Signal.mailbox True

slot3DesiredBox : Signal.Mailbox Bool
slot3DesiredBox = Signal.mailbox False

slot4DesiredBox : Signal.Mailbox Bool
slot4DesiredBox = Signal.mailbox True

slot5DesiredBox : Signal.Mailbox Bool
slot5DesiredBox = Signal.mailbox False

slot6DesiredBox : Signal.Mailbox Bool
slot6DesiredBox = Signal.mailbox False

slot7DesiredBox : Signal.Mailbox Bool
slot7DesiredBox = Signal.mailbox False

slot8DesiredBox : Signal.Mailbox Bool
slot8DesiredBox = Signal.mailbox False

slot9DesiredBox : Signal.Mailbox Bool
slot9DesiredBox = Signal.mailbox False

slot10DesiredBox : Signal.Mailbox Bool
slot10DesiredBox = Signal.mailbox False

slot1checkbox : Signal Element
slot1checkbox = checkbox (Signal.message slot1DesiredBox.address) <~ slot1DesiredBox.signal

slot2checkbox : Signal Element
slot2checkbox = checkbox (Signal.message slot2DesiredBox.address) <~ slot2DesiredBox.signal

slot3checkbox : Signal Element
slot3checkbox = checkbox (Signal.message slot3DesiredBox.address) <~ slot3DesiredBox.signal

slot4checkbox : Signal Element
slot4checkbox = checkbox (Signal.message slot4DesiredBox.address) <~ slot4DesiredBox.signal

slot5checkbox : Signal Element
slot5checkbox = checkbox (Signal.message slot5DesiredBox.address) <~ slot5DesiredBox.signal

slot6checkbox : Signal Element
slot6checkbox = checkbox (Signal.message slot6DesiredBox.address) <~ slot6DesiredBox.signal

slot7checkbox : Signal Element
slot7checkbox = checkbox (Signal.message slot7DesiredBox.address) <~ slot7DesiredBox.signal

slot8checkbox : Signal Element
slot8checkbox = checkbox (Signal.message slot8DesiredBox.address) <~ slot8DesiredBox.signal

slot9checkbox : Signal Element
slot9checkbox = checkbox (Signal.message slot9DesiredBox.address) <~ slot9DesiredBox.signal

slot10checkbox : Signal Element
slot10checkbox = checkbox (Signal.message slot10DesiredBox.address) <~ slot10DesiredBox.signal

desiredSlotsInputs : Signal Element
desiredSlotsInputs = flow right <~ combine
    [ slot1checkbox
    , (.slot1
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot2checkbox
    , (.slot2
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot3checkbox
    , (.slot3
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot4checkbox
    , (.slot4
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot5checkbox
    , (.slot5
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot6checkbox
    , (.slot6
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot7checkbox
    , (.slot7
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot8checkbox
    , (.slot8
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot9checkbox
    , (.slot9
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    , slot10checkbox
    , (.slot10
        >> displayName
        >> Text.fromString
        >> centered
      ) <~ encounterTable
    ]

desiredSlots : Signal (List Int)
desiredSlots =
    (\slot1
      slot2
      slot3
      slot4
      slot5
      slot6
      slot7
      slot8
      slot9
      slot10 ->
    [ (1, slot1)
    , (2, slot2)
    , (3, slot3)
    , (4, slot4)
    , (5, slot5)
    , (6, slot6)
    , (7, slot7)
    , (8, slot8)
    , (9, slot9)
    , (10, slot10)
    ]
    |> List.filter snd
    |> List.map fst
    )
    <~ slot1DesiredBox.signal
    ~ slot2DesiredBox.signal
    ~ slot3DesiredBox.signal
    ~ slot4DesiredBox.signal
    ~ slot5DesiredBox.signal
    ~ slot6DesiredBox.signal
    ~ slot7DesiredBox.signal
    ~ slot8DesiredBox.signal
    ~ slot9DesiredBox.signal
    ~ slot10DesiredBox.signal

leadPokemon : Signal Species
leadPokemon = Signal.constant (Maybe.withDefault noSpecies (Dict.get "Squirtle" speciesByName))

buildRequestList : EncounterTable -> List Int -> Species -> List (String, ChartRequest)
buildRequestList table slots poke =
    [ ( displayName table.slot1
      , { desiredSlots = slots
        , encounteredSlots = [1]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot1.species
        }
      )
    , ( displayName table.slot2
      , { desiredSlots = slots
        , encounteredSlots = [2]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot2.species
        }
      )
    , ( displayName table.slot3
      , { desiredSlots = slots
        , encounteredSlots = [3]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot3.species
        }
      )
    , ( displayName table.slot4
      , { desiredSlots = slots
        , encounteredSlots = [4]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot4.species
        }
      )
    , ( displayName table.slot5
      , { desiredSlots = slots
        , encounteredSlots = [5]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot5.species
        }
      )
    , ( displayName table.slot6
      , { desiredSlots = slots
        , encounteredSlots = [6]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot6.species
        }
      )
    , ( displayName table.slot7
      , { desiredSlots = slots
        , encounteredSlots = [7]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot7.species
        }
      )
    , ( displayName table.slot8
      , { desiredSlots = slots
        , encounteredSlots = [8]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot8.species
        }
      )
    , ( displayName table.slot9
      , { desiredSlots = slots
        , encounteredSlots = [9]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot9.species
        }
      )
    , ( displayName table.slot10
      , { desiredSlots = slots
        , encounteredSlots = [10]
        , encounterRate = table.rate
        , encounterLength = battleLength poke table.slot10.species
        }
      )
    ]

partialRequestBox : Signal.Mailbox ChartRequest
partialRequestBox = 
    Signal.mailbox
        { desiredSlots = [2, 4]
        , encounteredSlots = [1]
        , encounterRate = 25
        , encounterLength = 594
        }

requestSignal : Signal ChartRequest
requestSignal =
    (\partialRequest
      slots ->
    { partialRequest
    | desiredSlots <- slots
    }
    ) <~ partialRequestBox.signal ~ desiredSlots

requestDropDown : Signal Element
requestDropDown =
    dropDown (Signal.message partialRequestBox.address)
    <~ (buildRequestList <~ encounterTable ~ desiredSlots ~ leadPokemon)

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

type alias ChartRequest =
    { desiredSlots : List Int
    , encounteredSlots : List Int
    , encounterRate : Int
    , encounterLength : Int
    }

approxInputSignal : Signal (ChartRequest, Int, DApproxState, List Float)
approxInputSignal = (\req ->
    let
    initialState =
        initialDApproxState
        |> conditionDApprox (\x ->
            dsumSlotDist req.encounterRate x
            |> Dist.probability (\s -> List.member s req.encounteredSlots))
        |> advanceDApprox insideSlopeDist req.encounterLength
        |> advanceDApprox outsideSlopeDist framesBeforeMove
        |> (\s -> { s | muDist <- Dist.map (toFloat << round) s.muDist })
    in
    (req, 0, initialState, [])
    ) <~ Signal.sampleOn calculateBox.signal requestSignal

approxProbabilitiesWorker : Worker (ChartRequest, Int, DApproxState, List Float) (List Float)
approxProbabilitiesWorker =
    let
    workerStep (req, n, state, acc) =
        let frameState = advanceDApprox outsideSlopeDist n state
        in if n < 1000
        then Working
            (req, n+1, state, approxProbability req.encounterRate req.desiredSlots frameState :: acc)
        else Worker.Done (List.reverse acc)
    in
    createWorker approxInputSignal (iterateStateFunc 2 workerStep)

approxProbabilitiesSignal : Signal (List Float)
approxProbabilitiesSignal = Signal.map (\state ->
    case snd state of
        Working (_, _, _, acc) -> List.reverse acc
        Worker.Done probs -> probs
        Unstarted -> []
    ) approxProbabilitiesWorker.state

approxGraph : Signal Graph
approxGraph = graph (Just (0, 1000)) (Just (0, 1)) << (\x -> [x]) << toPath <~ approxProbabilitiesSignal

buildStrategy : Float -> List Float -> Strategy
buildStrategy threshold = simplify 15 << frameStrategy << List.map (\x -> x >= threshold)

strategy : Signal Strategy
strategy = buildStrategy <~ thresholdSignal ~ (Signal.map (Maybe.withDefault []) approxProbabilitiesWorker.signal)

strategy2 : Signal Strategy
strategy2 = roundStrategy 17 <~ strategy

stepStrategy : Signal (List (Int, Bool))
stepStrategy = List.map (\s -> (s.frames // 17, s.inGrass)) <~ strategy2

exampleQuery : Query
exampleQuery =
    { duration = 1000
    , successFunc = Dist.probability (\x -> List.member x [2, 4]) << dsumSlotDist 25 
    , initialSteps =
        [ QCondition (Dist.probability (\x -> x == 3) << dsumSlotDist 25)
        , QAdvance 595 insideSlopeDist
        , QAdvance framesBeforeMove outsideSlopeDist
        ]
    }

queryWorker : Worker QueryWorkerState (List Float)
queryWorker = createQueryWorker (Signal.constant exampleQuery)

queryProbabilitiesSignal : Signal (List Float)
queryProbabilitiesSignal = Signal.map (\state ->
    case snd state of
        Working (_, _, _, acc) -> List.reverse acc
        Worker.Done probs -> probs
        Unstarted -> []
    ) queryWorker.state

queryGraph : Signal Graph
queryGraph = graph (Just (0, 1000)) (Just (0, 1)) << (\x -> [x]) << toPath <~ queryProbabilitiesSignal

main : Signal Element
main = flow down <~ combine
    [ requestDropDown
    , desiredSlotsInputs
    , Signal.constant calculateButton
    , thresholdInput
    , drawGraph 700 400 <~ approxGraph
    , drawGraph 700 400 <~ queryGraph
    , Signal.map show strategy
    , Signal.map show strategy2
    , Signal.map show stepStrategy
    ]
