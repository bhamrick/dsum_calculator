module Query where

import List
import Signal exposing ((<~))

import DApprox exposing (..)
import Dist exposing (..)
import Worker exposing (..)

type QueryStep
    = QCondition (Int -> Float)
    | QAdvance Int (Dist Float)

type alias Query =
    { duration : Int
    , offset : Int
    , successFunc : Int -> Float
    , initialSteps : List QueryStep
    }
type alias QueryWorkerState = (Query, Int, DApproxState, List Float)

queryStep : QueryStep -> DApproxState -> DApproxState
queryStep q s = case q of
    QCondition f -> conditionDApprox f s
    QAdvance n slopeDist -> advanceDApprox slopeDist n s

queryDApproxState : List QueryStep -> DApproxState
queryDApproxState l = List.foldl queryStep initialDApproxState l

queryWorkerStep : QueryWorkerState -> WorkerState QueryWorkerState (List Float)
queryWorkerStep (q, n, s, acc) =
    if n >= q.duration
    then Done (List.reverse acc)
    else
        let
        frameState = advanceDApprox outsideSlopeDist (n + q.offset) s
        frameSuccessProb = weightedProbability q.successFunc (dapproxDist frameState)
        in
        Working (q, n+1, s, frameSuccessProb :: acc)

createQueryWorker : Signal Query -> Worker QueryWorkerState (List Float)
createQueryWorker signal =
    let
    inputSignal = (\q -> (q, 0, queryDApproxState q.initialSteps, [])) <~ signal
    in
    createWorker inputSignal queryWorkerStep
