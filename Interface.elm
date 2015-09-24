module Interface where

import Debug
import Dict
import List
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Input.Field exposing (..)
import Signal exposing (Mailbox, Message, Signal)
import String
import Text exposing (..)

import DApprox exposing (..)
import Dist
import DSum exposing (..)
import Encounters exposing (..)
import Pokemon exposing (..)
import Query exposing (..)

type InterfaceStep
    = IEncounter
        { table : EncounterTable
        , slot1 : Bool
        , slot2 : Bool
        , slot3 : Bool
        , slot4 : Bool
        , slot5 : Bool
        , slot6 : Bool
        , slot7 : Bool
        , slot8 : Bool
        , slot9 : Bool
        , slot10 : Bool
        }
    | IWalk
        { frames : Int
        , frameContent : Content
        }

defaultIEncounter : InterfaceStep
defaultIEncounter =
    IEncounter
        { table = case (List.head allTables) of
            Just t -> t
            Nothing -> Debug.crash "No encounter tables!"
        , slot1 = False
        , slot2 = False
        , slot3 = False
        , slot4 = False
        , slot5 = False
        , slot6 = False
        , slot7 = False
        , slot8 = False
        , slot9 = False
        , slot10 = False
        }

defaultIWalk : InterfaceStep
defaultIWalk =
    IWalk
        { frames = 0
        , frameContent = noContent
        }

type alias InterfaceQuery = List (Int, InterfaceStep)
type alias InterfaceState =
    { nextId : Int
    , query : InterfaceQuery
    }
type InterfaceStateDelta
    = EditStep Int InterfaceStep
    | AddStep
    | RemoveStep Int

defaultInterfaceState : InterfaceState
defaultInterfaceState =
    { nextId = 0
    , query = []
    }

stepField : (InterfaceStep -> Message) -> InterfaceStep -> Element
stepField sendStep step =
    let
    innerField = case step of
        IEncounter enc ->
            let
            sendTable table =
                sendStep (IEncounter { enc | table <- table })
            sendSlot1 b =
                sendStep (IEncounter { enc | slot1 <- b })
            sendSlot2 b =
                sendStep (IEncounter { enc | slot2 <- b })
            sendSlot3 b =
                sendStep (IEncounter { enc | slot3 <- b })
            sendSlot4 b =
                sendStep (IEncounter { enc | slot4 <- b })
            sendSlot5 b =
                sendStep (IEncounter { enc | slot5 <- b })
            sendSlot6 b =
                sendStep (IEncounter { enc | slot6 <- b })
            sendSlot7 b =
                sendStep (IEncounter { enc | slot7 <- b })
            sendSlot8 b =
                sendStep (IEncounter { enc | slot8 <- b })
            sendSlot9 b =
                sendStep (IEncounter { enc | slot9 <- b })
            sendSlot10 b =
                sendStep (IEncounter { enc | slot10 <- b })
            in
            flow down
                [ dropDown sendTable (List.map (\t -> (t.name, t)) allTables)
                , flow right
                    [ checkbox sendSlot1 enc.slot1
                    , leftAligned (fromString (displayName enc.table.slot1))
                    ]
                , flow right
                    [ checkbox sendSlot2 enc.slot2
                    , leftAligned (fromString (displayName enc.table.slot2))
                    ]
                , flow right
                    [ checkbox sendSlot3 enc.slot3
                    , leftAligned (fromString (displayName enc.table.slot3))
                    ]
                , flow right
                    [ checkbox sendSlot4 enc.slot4
                    , leftAligned (fromString (displayName enc.table.slot4))
                    ]
                , flow right
                    [ checkbox sendSlot5 enc.slot5
                    , leftAligned (fromString (displayName enc.table.slot5))
                    ]
                , flow right
                    [ checkbox sendSlot6 enc.slot6
                    , leftAligned (fromString (displayName enc.table.slot6))
                    ]
                , flow right
                    [ checkbox sendSlot7 enc.slot7
                    , leftAligned (fromString (displayName enc.table.slot7))
                    ]
                , flow right
                    [ checkbox sendSlot8 enc.slot8
                    , leftAligned (fromString (displayName enc.table.slot8))
                    ]
                , flow right
                    [ checkbox sendSlot9 enc.slot9
                    , leftAligned (fromString (displayName enc.table.slot9))
                    ]
                , flow right
                    [ checkbox sendSlot10 enc.slot10
                    , leftAligned (fromString (displayName enc.table.slot10))
                    ]
                ]
        IWalk w ->
            let
            sendContent c =
                case String.toInt (c.string) of
                    Ok n -> sendStep (IWalk { w | frames <- n, frameContent <- c })
                    Err _ -> sendStep (IWalk { w | frameContent <- c })
            in
            flow right
                [ field Graphics.Input.Field.defaultStyle sendContent "0" w.frameContent
                , leftAligned (fromString "frames")
                ]
    in
    flow right
        [ dropDown sendStep
            [ ("Encounter", defaultIEncounter)
            , ("Walk/Wait", defaultIWalk)
            ]
        , innerField
        ]

updateInterfaceState : InterfaceStateDelta -> InterfaceState -> InterfaceState
updateInterfaceState delta state =
    case delta of
        EditStep n newStep ->
            { state
            | query <- List.map (\(i, step) ->
                    if i == n
                    then (i, newStep)
                    else (i, step)
                ) state.query
            }
        AddStep ->
            { state
            | nextId <- state.nextId + 1
            , query <- List.append state.query [(state.nextId, defaultIEncounter)]
            }
        RemoveStep n ->
            { state
            | query <- List.filter (\(i, _) -> i /= n) state.query
            }

queryInterface : (InterfaceState -> Message) -> InterfaceState -> Element
queryInterface sendState state =
    let
    sendDelta delta = sendState (updateInterfaceState delta state)
    sendStep i step = sendDelta (EditStep i step)
    in
    flow right
        [ flow down
            [ button (sendDelta AddStep) "Add step"
            ]
        , flow down (List.map (\(i, step) ->
            flow right
                [ stepField (sendStep i) step
                , button (sendDelta (RemoveStep i)) "Remove"
                ]
          ) state.query)
        ]

successFunc :
    { table : EncounterTable
    , slot1 : Bool
    , slot2 : Bool
    , slot3 : Bool
    , slot4 : Bool
    , slot5 : Bool
    , slot6 : Bool
    , slot7 : Bool
    , slot8 : Bool
    , slot9 : Bool
    , slot10 : Bool
    } -> Int -> Float
successFunc enc =
    let
        desiredSlots =
            [ enc.slot1
            , enc.slot2
            , enc.slot3
            , enc.slot4
            , enc.slot5
            , enc.slot6
            , enc.slot7
            , enc.slot8
            , enc.slot9
            , enc.slot10
            ]
            |> List.map2 (,) [ 1 .. 10]
            |> List.filter snd
            |> List.map fst
    in
    Dist.probability (\x -> List.member x desiredSlots) << dsumSlotDist enc.table.rate

encSpecies : 
    { table : EncounterTable
    , slot1 : Bool
    , slot2 : Bool
    , slot3 : Bool
    , slot4 : Bool
    , slot5 : Bool
    , slot6 : Bool
    , slot7 : Bool
    , slot8 : Bool
    , slot9 : Bool
    , slot10 : Bool
    } -> Species
encSpecies enc = if
    | enc.slot1 -> enc.table.slot1.species
    | enc.slot2 -> enc.table.slot2.species
    | enc.slot3 -> enc.table.slot3.species
    | enc.slot4 -> enc.table.slot4.species
    | enc.slot5 -> enc.table.slot5.species
    | enc.slot6 -> enc.table.slot6.species
    | enc.slot7 -> enc.table.slot7.species
    | enc.slot8 -> enc.table.slot8.species
    | enc.slot9 -> enc.table.slot9.species
    | enc.slot10 -> enc.table.slot10.species
    | otherwise -> noSpecies

squirtle : Species
squirtle = speciesByName |> Dict.get "Squirtle" |> Maybe.withDefault noSpecies

buildQueryStep : InterfaceStep -> (Int, List QueryStep) -> (Int, List QueryStep)
buildQueryStep step (n, acc) =
    case step of
        IEncounter enc ->
            let
            acc' =
                if n == 0
                then acc
                else QAdvance n outsideSlopeDist :: acc
            encStep =
                QCondition (successFunc enc)
            battleStep =
                QAdvance (battleLength squirtle (encSpecies enc)) insideSlopeDist
            in
            (framesBeforeMove, battleStep :: encStep :: acc')
        IWalk w -> (w.frames + n, acc)

buildQuery : InterfaceState -> Query
buildQuery s =
    let
    (offset, revSteps) = List.foldl buildQueryStep (0, []) (List.map snd s.query)
    in
    { duration = 1000
    , offset = offset
    , successFunc = Dist.probability (\x -> List.member x [2, 4]) << dsumSlotDist 25
    , initialSteps = List.reverse revSteps
    }
