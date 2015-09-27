module Interface where

import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List
import Signal exposing (Mailbox, Message, Signal)
import String

import DApprox exposing (..)
import Dist
import DSum exposing (..)
import Encounters exposing (..)
import Pokemon exposing (..)
import Query exposing (..)

type alias EncounterSlots =
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
type InterfaceStep
    = IEncounter EncounterSlots
    | IWalk
        { frames : Int
        }

defaultEncounterSlots =
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

defaultIEncounter : InterfaceStep
defaultIEncounter =
    IEncounter defaultEncounterSlots

defaultIWalk : InterfaceStep
defaultIWalk =
    IWalk
        { frames = 0
        }

type alias InterfaceQuery = List (Int, InterfaceStep)
type alias InterfaceState =
    { nextId : Int
    , query : InterfaceQuery
    , lead : Species
    , desire : EncounterSlots
    , duration : Int
    }
type InterfaceStateDelta
    = EditStep Int InterfaceStep
    | AddStep
    | RemoveStep Int
    | EditDesire EncounterSlots
    | EditLead Species
    | EditDuration Int

defaultInterfaceState : InterfaceState
defaultInterfaceState =
    { nextId = 0
    , query = []
    , lead = speciesByPokedex
        |> Dict.get 1
        |> Maybe.withDefault noSpecies
    , desire = defaultEncounterSlots
    , duration = 1000
    }

checkbox : (Bool -> Message) -> Bool -> Html
checkbox f b = input
    [ type' "checkbox"
    , on "change" targetChecked f
    , checked b
    ]
    []

unsafeFromJust : String -> Maybe a -> a
unsafeFromJust s x =
    case x of
        Nothing -> Debug.crash s
        Just x' -> x'

dropdown : (a -> Message) -> List (String, a) -> Html
dropdown send options =
    let
    valueDict =
        options
        |> List.map snd
        |> List.map2 (,) [0 .. List.length options - 1]
        |> Dict.fromList
    in
    select
        [ on "change" (Json.at ["target", "selectedIndex"] Json.int)
            (\i ->
                valueDict
                |> Dict.get i
                |> unsafeFromJust ("Unexpected index from dropdown: " ++ toString i)
                |> send
            )
        , style [("display", "inline-block")]
        ]
        (List.map (\(s, _) ->
            option
                []
                [ text s ]
        ) options)

encounterSlotsField : (EncounterSlots -> Message) -> EncounterSlots -> Html
encounterSlotsField send enc =
    let
    sendTable table =
        send { enc | table <- table }
    sendSlot1 b =
        send { enc | slot1 <- b }
    sendSlot2 b =
        send { enc | slot2 <- b }
    sendSlot3 b =
        send { enc | slot3 <- b }
    sendSlot4 b =
        send { enc | slot4 <- b }
    sendSlot5 b =
        send { enc | slot5 <- b }
    sendSlot6 b =
        send { enc | slot6 <- b }
    sendSlot7 b =
        send { enc | slot7 <- b }
    sendSlot8 b =
        send { enc | slot8 <- b }
    sendSlot9 b =
        send { enc | slot9 <- b }
    sendSlot10 b =
        send { enc | slot10 <- b }
    in
    div [ style [("display", "inline-block")] ]
        [ table []
            [ tr []
                [ td []
                    [ dropdown sendTable (List.map (\t -> (t.name, t)) allTables) ]
                , td []
                    [ checkbox sendSlot1 enc.slot1
                    , text (displayName enc.table.slot1)
                    ]
                , td []
                    [ checkbox sendSlot2 enc.slot2
                    , text (displayName enc.table.slot2)
                    ]
                , td []
                    [ checkbox sendSlot3 enc.slot3
                    , text (displayName enc.table.slot3)
                    ]
                , td []
                    [ checkbox sendSlot4 enc.slot4
                    , text (displayName enc.table.slot4)
                    ]
                , td []
                    [ checkbox sendSlot5 enc.slot5
                    , text (displayName enc.table.slot5)
                    ]
                ]
            , tr []
                [ td [] []
                , td []
                    [ checkbox sendSlot6 enc.slot6
                    , text (displayName enc.table.slot6)
                    ]
                , td []
                    [ checkbox sendSlot7 enc.slot7
                    , text (displayName enc.table.slot7)
                    ]
                , td []
                    [ checkbox sendSlot8 enc.slot8
                    , text (displayName enc.table.slot8)
                    ]
                , td []
                    [ checkbox sendSlot9 enc.slot9
                    , text (displayName enc.table.slot9)
                    ]
                , td []
                    [ checkbox sendSlot10 enc.slot10
                    , text (displayName enc.table.slot10)
                    ]
                ]
            ]
        ]

stepField : (InterfaceStep -> Message) -> InterfaceStep -> Html
stepField sendStep step =
    let
    innerField = case step of
        IEncounter enc ->
            encounterSlotsField (sendStep << IEncounter) enc
        IWalk w ->
            let
            sendContent c =
                case String.toInt c of
                    Ok n -> sendStep (IWalk { w | frames <- n })
                    Err _ -> sendStep (IWalk w)
            in
            div [ style [("display", "inline-block"), ("margin", "2px")] ]
                [ input
                    [ type' "text"
                    , on "change" targetValue sendContent
                    , placeholder "0"
                    , value (toString w.frames)
                    ]
                    []
                , text "frames"
                ]
    in
    div
        [ style [("display", "inline-block")] ]
        [ div 
            [ style [("display", "inline-block"), ("margin", "3px"), ("vertical-align", "top")] ]
            [ dropdown sendStep [("Encounter", defaultIEncounter), ("Walk/Wait", defaultIWalk)] ]
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
        EditDesire enc ->
            { state
            | desire <- enc
            }
        EditLead species ->
            { state
            | lead <- species
            }
        EditDuration dur ->
            { state
            | duration <- dur
            }

queryInterface : (InterfaceState -> Message) -> InterfaceState -> Html
queryInterface sendState state =
    let
    sendDelta delta = sendState (updateInterfaceState delta state)
    sendStep i step = sendDelta (EditStep i step)
    sendDuration durString =
        case String.toInt durString of
            Err _ -> sendDelta (EditDuration state.duration)
            Ok dur -> sendDelta (EditDuration dur)
    in
    table []
        [ tr []
            [ td []
                [ text "Previous Encounters" ]
            , td []
                [ div [] (List.map (\(i, step) ->
                    div []
                        [ stepField (sendStep i) step
                        , input
                            [ type' "button"
                            , on "click" Json.value (\_ -> sendDelta (RemoveStep i))
                            , value "Remove"
                            , style 
                                [ ("display", "inline-block")
                                , ("vertical-align", "top")
                                , ("margin", "3px")
                                ]
                            ]
                            []
                        ]
                  ) state.query)
                , div []
                    [ input
                        [ type' "button"
                        , on "click" Json.value (\_ -> sendDelta AddStep)
                        , value "Add step"
                        , style [("margin", "1px")]
                        ]
                        []
                    ]
                ]
            ]
        , tr []
            [ td []
                [ text "Lead Pokemon" ]
            , td
                [ style [("padding", "3px")] ]
                [ speciesList
                    |> List.sortBy .pokedexNumber
                    |> List.map (\s -> (s.name, s))
                    |> dropdown (sendDelta << EditLead)
                ]
            ]
        , tr []
            [ td []
                [ text "Desired Encounters" ]
            , td []
                [ encounterSlotsField (sendDelta << EditDesire) state.desire ]
            ]
        , tr []
            [ td []
                [ text "Duration" ]
            , td []
                [ input
                    [ type' "text"
                    , on "change" targetValue sendDuration
                    , value (toString state.duration)
                    ]
                    []
                , text "frames"
                ]
            ]
        ]

successFunc : EncounterSlots -> Int -> Float
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
            |> List.map2 (,) [1 .. 10]
            |> List.filter snd
            |> List.map fst
    in
    Dist.probability (\x -> List.member x desiredSlots) << dsumSlotDist enc.table.rate

encSpecies : EncounterSlots -> Species
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
    { duration = s.duration
    , offset = offset
    , successFunc = successFunc s.desire
    , initialSteps = List.reverse revSteps
    }
