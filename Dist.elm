module Dist where

import Dict exposing (Dict)
import List

type alias Dist a = List (a, Float)

probability : (a -> Bool) -> Dist a -> Float
probability f dist = List.foldl (\(x, p) s -> if f x then s + p else s) 0 dist

weightedProbability : (a -> Float) -> Dist a -> Float
weightedProbability f dist = List.foldl (\(x, p) s -> s + f x * p) 0 dist

always : a -> Dist a
always x = [(x, 1)]

combineProbsStep : (comp, Float) -> (comp, Float, List (comp, Float)) -> (comp, Float, List (comp, Float))
combineProbsStep (x0, p0) (x1, p1, acc) =
    if x0 == x1
    then (x1, p0 + p1, acc)
    else (x0, p0, (x1, p1) :: acc)

combineProbs : List (comp, Float) -> List (comp, Float)
combineProbs l = case l of
    [] -> []
    (x0, p0) :: rest ->
        let (y0, q0, yqs) = List.foldl combineProbsStep (x0, p0, []) rest
        in (y0, q0) :: yqs

map : (comparable1 -> comparable2) -> Dist comparable1 -> Dist comparable2
map f dist = dist
    |> List.map (\(k, v) -> (f k, v))
    |> List.sort
    |> combineProbs

normalize : List (comparable, Float) -> List (comparable, Float)
normalize probs =
    let total = List.sum (List.map snd probs)
    in
    probs
    |> List.map (\(k, v) -> (k, v / total))
    |> List.filter (\(k, v) -> v /= 0)

filter : (comparable -> Bool) -> Dist comparable -> Dist comparable
filter f dist = dist
    |> List.filter (\(k, _) -> f k)
    |> normalize

condition : (comparable -> Float) -> Dist comparable -> Dist comparable
condition f dist = dist
    |> List.map (\(k, p) -> (k, p * f k))
    |> normalize

product' : List (a, Float) -> List (b, Float) -> List ((a, b), Float)
product' l1 l2 = case l1 of
    [] -> []
    (x, p)::rest ->
        let
        probs = List.map (\(y, q) -> ((x, y), p*q)) l2
        in
        List.append probs (product' rest l2)

product : Dist comparable1 -> Dist comparable2 -> Dist (comparable1, comparable2)
product = product'

-- TODO: Write as a fold?
lift2' : (a -> b -> c) -> List (a, Float) -> List (b, Float) -> List (c, Float)
lift2' f l1 l2 = case l1 of
    [] -> []
    (x, p)::rest ->
        let
        probs = List.map (\(y, q) -> (f x y, p * q)) l2
        in List.append probs (lift2' f rest l2)

lift2 : (comparable1 -> comparable2 -> comparable3) -> Dist comparable1 -> Dist comparable2 -> Dist comparable3
lift2 f d1 d2 =
    lift2' f d1 d2
    |> List.sort
    |> combineProbs

uniform : List comparable -> Dist comparable
uniform l = let n = List.length l in l
    |> List.map (\x -> (x, 1/(toFloat n)))

collapse' : List (List (x, Float), Float) -> List (x, Float)
collapse' vals = List.concat (List.map (\(l, p) -> List.map (\(x, q) -> (x, p*q)) l) vals)

collapseMap : (comparable1 -> Dist comparable2) -> Dist comparable1 -> Dist comparable2
collapseMap f dist =
    let
    nestedProbs = List.map (\(x, p) -> (f x, p)) dist
    collapsedProbs = collapse' nestedProbs
    in
    collapsedProbs
        |> List.sort
        |> combineProbs
