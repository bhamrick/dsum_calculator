module Timing where

type TimingInterval = Frame | Second | Step -- | Beat

numFrames : TimingInterval -> Float
numFrames interval = case interval of
    Frame -> 1
    Second -> 262144 / 4389
    Step -> 17
