module Dist where

import Dict exposing (Dict)
import List

type alias Dist comparable = Dict comparable Float

probability : (comparable -> Bool) -> Dist comparable -> Float
probability f dist = Dict.foldl (\x p s -> if f x then s + p else s) 0 dist

weightedProbability : (comparable -> Float) -> Dist comparable -> Float
weightedProbability f dist = Dict.foldl (\x p s -> s + f x * p) 0 dist

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

map : (comparable1 -> comparable2) -> Dist comparable1 -> Dist comparable2
map f dist = dist
    |> Dict.toList
    |> List.map (\(k, v) -> (f k, v))
    |> List.sort
    |> combineProbs
    |> Dict.fromList

normalize : List (comparable, Float) -> List (comparable, Float)
normalize probs =
    let total = List.sum (List.map snd probs)
    in
    probs
    |> List.map (\(k, v) -> (k, v / total))
    |> List.filter (\(k, v) -> v /= 0)

filter : (comparable -> Bool) -> Dist comparable -> Dist comparable
filter f dist = dist
    |> Dict.toList
    |> List.filter (\(k, _) -> f k)
    |> normalize
    |> Dict.fromList

condition : (comparable -> Float) -> Dist comparable -> Dist comparable
condition f dist = dist
    |> Dict.toList
    |> List.map (\(k, p) -> (k, p * f k))
    |> normalize
    |> Dict.fromList

product' : List (a, Float) -> List (b, Float) -> List ((a, b), Float)
product' l1 l2 = case l1 of
    [] -> []
    (x, p)::rest ->
        let
        probs = List.map (\(y, q) -> ((x, y), p*q)) l2
        in
        List.append probs (product' rest l2)

product : Dist comparable1 -> Dist comparable2 -> Dist (comparable1, comparable2)
product d1 d2 =
    let
    probs1 = Dict.toList d1
    probs2 = Dict.toList d2
    productProbs = product' probs1 probs2
    in
    Dict.fromList productProbs

lift2 : (comparable1 -> comparable2 -> comparable3) -> Dist comparable1 -> Dist comparable2 -> Dist comparable3
lift2 f d1 d2 = map (uncurry f) (product d1 d2)

uniform : List comparable -> Dist comparable
uniform l = let n = List.length l in l
    |> List.map (\x -> (x, 1/(toFloat n)))
    |> Dict.fromList

collapse' : List (List (x, Float), Float) -> List (x, Float)
collapse' vals = List.concat (List.map (\(l, p) -> List.map (\(x, q) -> (x, p*q)) l) vals)

collapseMap : (comparable1 -> Dist comparable2) -> Dist comparable1 -> Dist comparable2
collapseMap f dist =
    let
    probs = Dict.toList dist
    nestedProbs = List.map (\(x, p) -> (Dict.toList (f x), p)) probs
    collapsedProbs = collapse' nestedProbs
    in
    collapsedProbs
        |> List.sort
        |> combineProbs
        |> Dict.fromList
