module Dist where

import Dict exposing (Dict)
import List

type alias Dist comparable = List (comparable, Float)

probability : (comparable -> Bool) -> Dist comparable -> Float
probability f dist = List.foldl (\(x, p) s -> if f x then s + p else s) 0 dist

weightedProbability : (comparable -> Float) -> Dist comparable -> Float
weightedProbability f dist = List.foldl (\(x, p) s -> s + f x * p) 0 dist

{-
partitionPrefix : (a -> Bool) -> List a -> (List a, List a)
partitionPrefix f l = case l of
    [] -> ([], [])
    x::xs -> if f x
        then let (matches, rest) = partitionPrefix f xs in (x::matches, rest)
        else ([], l)

combineProbs : List (comp, Float) -> List (comp, Float)
combineProbs list = case list of
    [] -> []
    (k, v) :: _ ->
        let (matches, rest) = partitionPrefix ((==) k << fst) list
        in (k, List.sum << List.map snd <| matches) :: combineProbs rest
-}

-- TODO: Figure out if this can be a fold, since those are compiled as pure loops
combineProbs' : comp -> Float -> List (comp, Float) -> List (comp, Float)
combineProbs' x0 p0 l = case l of
    [] -> [(x0, p0)]
    (x1, p1) :: rest ->
        if x0 == x1
        then combineProbs' x0 (p0 + p1) rest
        else (x0, p0) :: combineProbs' x1 p1 rest

combineProbs : List (comp, Float) -> List (comp, Float)
combineProbs l = case l of
    [] -> []
    (x0, p0) :: rest -> combineProbs' x0 p0 rest

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
