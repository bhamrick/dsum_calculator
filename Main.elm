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

import Dist
import DSum exposing (..)
import Encounters exposing (..)
import Graph exposing (..)
import RNG exposing (..)
import Pokemon exposing (..)
import Strategy exposing (..)
import Worker exposing (..)

encounterTable : Signal EncounterTable
encounterTable = Signal.constant route22table

desiredSlots : Signal (List Int)
desiredSlots = Signal.constant [2, 4]

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

requestBox : Signal.Mailbox ChartRequest
requestBox = 
    Signal.mailbox
        { desiredSlots = [2, 4]
        , encounteredSlots = [1]
        , encounterRate = 25
        , encounterLength = 594
        }

requestDropDown : Signal Element
requestDropDown =
    dropDown (Signal.message requestBox.address)
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
    | n == 0 -> Trampoline.Done x
    | otherwise -> Continue (\() -> iterate' (n-1) f (f x))

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

dsumGraph : Signal Graph
dsumGraph = graph (Just (0, 1638)) (Just (0, 255)) << List.map (toPath << List.map toFloat) << List.map sampleEncounterDSums <~ Signal.sampleOn calculateBox.signal initialRNGStates

type alias ChartRequest =
    { desiredSlots : List Int
    , encounteredSlots : List Int
    , encounterRate : Int
    , encounterLength : Int
    }

workerInputSignal : Signal (ChartRequest, Int, DSumState, List Float)
workerInputSignal = (\req ->
    let
    initialState =
        initialRNGMix
        |> conditionDSum (\x ->
            dsumSlotDist req.encounterRate x
            |> Dist.probability (\s -> List.member s req.encounteredSlots))
    in
    (req, 0, initialState, [])
    ) <~ Signal.sampleOn calculateBox.signal requestBox.signal

successProbabilitiesWorker : Worker (ChartRequest, Int, DSumState, List Float) (List Float)
successProbabilitiesWorker =
    let
    workerStep (req, n, state, acc) =
        let state' = if
                | n < req.encounterLength ->
                    dsumStep 1 state
                | n == req.encounterLength ->
                    dsumStep 1 state 
                    |> Dist.collapseMap randomizeBand
                | otherwise ->
                    dsumStep 0 state
            acc' = if
                | n < req.encounterLength + framesBeforeMove ->
                    acc
                | otherwise ->
                    successProbability req.encounterRate req.desiredSlots state :: acc
        in
        if n < req.encounterLength + framesBeforeMove + 1000
        then Working (req, n+1, state', acc')
        else Worker.Done (List.reverse acc)
    in
    createWorker workerInputSignal workerStep

successProbabilitiesSignal : Signal (List Float)
successProbabilitiesSignal = Signal.map (\state ->
    case snd state of
        Working (_, _, _, acc) -> List.reverse acc
        Worker.Done probs -> probs
        Unstarted -> []
    ) successProbabilitiesWorker.state


successGraph : Signal Graph
successGraph = graph (Just (0, 1000)) (Just (0, 1)) << (\x -> [x]) << toPath <~ successProbabilitiesSignal

buildStrategy : Float -> List Float -> Strategy
buildStrategy threshold = simplify 15 << frameStrategy << List.map (\x -> x >= threshold)

strategy : Signal Strategy
strategy = buildStrategy <~ thresholdSignal ~ (Signal.map (Maybe.withDefault []) successProbabilitiesWorker.signal)

strategy2 : Signal Strategy
strategy2 = roundStrategy 17 <~ strategy

stepStrategy : Signal (List (Int, Bool))
stepStrategy = List.map (\s -> (s.frames // 17, s.inGrass)) <~ strategy2

main : Signal Element
main = flow down <~ combine
    [ requestDropDown
    , Signal.constant calculateButton
    , thresholdInput
    -- , drawGraph 700 400 <~ dsumGraph
    , drawGraph 700 400 <~ successGraph
    , Signal.map show strategy
    , Signal.map show strategy2
    , Signal.map show stepStrategy
    , let
        showWorkerState state = case state of
            Unstarted -> show "Unstarted"
            Working (req, n, dist, acc) -> show (n, dist)
            Worker.Done _ -> show "Done"
      in
        Signal.map (snd >> showWorkerState) successProbabilitiesWorker.state
    ]
