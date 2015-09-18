module Worker where

import Debug
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
workerClock = Debug.watch "frame time" <~ fps 60

createWorker : Signal s -> (s -> WorkerState s r) -> Worker s r
createWorker inputSignal step =
    let state = 
        Signal.foldp
            (\inp (initial, state) -> if Just inp == initial
                then case state of
                    Working s -> (initial, step s)
                    Done r -> (initial, Done r)
                else (Just inp, Working inp)
            )
            (Nothing, Unstarted)
            (inputSignal |> Signal.sampleOn workerClock)
    in
    { state = state
    , signal =
        state |> Signal.map (getResult << snd) |> Signal.dropRepeats
    }
