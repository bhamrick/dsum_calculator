module Worker where

import Debug
import Maybe
import Signal exposing ((<~))
import Time exposing (Time, fps)

type WorkerState s r = Working s | Done r | Unstarted
type alias Worker s r =
    { state : Signal (Maybe s, WorkerState s r)
    , signal : Signal (Maybe r)
    }

isWorking : WorkerState s r -> Bool
isWorking state = case state of
    Working _ -> True
    Done _ -> False
    Unstarted -> False

getResult : WorkerState s r -> Maybe r
getResult state = case state of
    Working _ -> Nothing
    Done r -> Just r
    Unstarted -> Nothing

workerClock : Signal Time
workerClock = Debug.watch "frame time" <~ fps 30

iterateStateFunc : Int -> (s -> WorkerState s r) -> s -> WorkerState s r
iterateStateFunc n f s =
    if n <= 0
    then Working s
    else case f s of
        Working s' -> iterateStateFunc (n-1) f s'
        Done r -> Done r
        Unstarted -> Unstarted

filterFoldp : (a -> s -> Maybe s) -> s -> Signal a -> Signal s
filterFoldp f s0 inp =
    let
    f' x (_, s) =
        case f x s of
            Nothing -> (False, s)
            Just s' -> (True, s')
    in
    inp
    |> Signal.foldp f' (True, s0)
    |> Signal.filterMap (\(b, x) -> if b then Just x else Nothing) s0

createWorker : Signal s -> (s -> WorkerState s r) -> Worker s r
createWorker inputSignal step =
    let state = 
        filterFoldp
            (\inp (initial, state) -> case inp of
                Nothing -> case state of
                    Working s -> Just (initial, step s)
                    Done r -> Nothing
                    Unstarted -> Nothing
                Just inp' -> Just (inp, Working inp')
            )
            (Nothing, Unstarted)
            (Signal.map Just inputSignal
            |> Signal.merge (Signal.map (\_ -> Nothing) workerClock))
    in
    { state = state
    , signal =
        state |> Signal.map (getResult << snd)
    }
