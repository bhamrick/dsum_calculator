module Graph where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List
import Maybe exposing (withDefault)
import Text

type alias Graph =
    { xRange : Maybe (Float, Float)
    , yRange : Maybe (Float, Float)
    , points : List (List (Float, Float))
    }

graph : Maybe (Float, Float) -> Maybe (Float, Float) -> List (List (Float, Float)) -> Graph
graph xRange yRange points =
    { xRange = xRange
    , yRange = yRange
    , points = points
    }

addSeries : List (Float, Float) -> Graph -> Graph
addSeries series g = { g | points <- series :: g.points }

gapSize : Float -> Float
gapSize x =
    let
    powerOfTen = 10 ^ toFloat (floor (logBase 10 x))
    in
    if
        | 5*powerOfTen < x -> 5*powerOfTen
        | 2*powerOfTen < x -> 2*powerOfTen
        | otherwise -> powerOfTen

roundTo : Float -> Float -> Float
roundTo gap x = gap * toFloat (round (x / gap))

labelPositions : Float -> Float -> List Float
labelPositions lo hi =
    let
        gap = gapSize ((hi - lo) / 2)
    in
    lo :: hi :: List.map ((*) gap << toFloat) [ceiling (lo / gap + 0.2) .. floor (hi / gap - 0.2)]

drawGraph : Int -> Int -> Graph -> Element
drawGraph w h g =
    let 
        minX = case g.xRange of
            Nothing -> withDefault 0 (List.minimum (List.map fst (List.concat g.points)))
            Just (l, _) -> l
        maxX = case g.xRange of
            Nothing -> withDefault 100 (List.maximum (List.map fst (List.concat g.points)))
            Just (_, h) -> h
        minY = case g.yRange of
            Nothing -> withDefault 0 (List.minimum (List.map snd (List.concat g.points)))
            Just (l, _) -> l
        maxY = case g.yRange of
            Nothing -> withDefault 100 (List.maximum (List.map snd (List.concat g.points)))
            Just (_, h) -> h
        graphOffsetX = 40
        graphOffsetY = 20
        graphW = toFloat w - 2*graphOffsetX
        graphH = toFloat h - 2*graphOffsetY
        pixelX x = (x - minX) / (maxX - minX) * graphW - (graphW / 2)
        pixelY y = (y - minY) / (maxY - minY) * graphH - (graphH / 2)
        pixelCoordinates x y = (pixelX x, pixelY y)
        color_cycle = [red, darkGreen, blue, purple, orange]
        colors = List.concat (List.repeat (List.length g.points // List.length color_cycle + 1) color_cycle)
        pointForm color (x, y) = move (pixelX x, pixelY y) (filled color (oval 1 1))
        seriesForm color points = List.map (pointForm color) points
        axisStyle = { defaultLine | width <- 2 }
        xAxis = traced axisStyle (segment (pixelCoordinates minX 0) (pixelCoordinates maxX 0))
        yAxis = traced axisStyle (segment (pixelCoordinates 0 minY) (pixelCoordinates 0 maxY))
        xLabel x = x
            |> toString
            |> Text.fromString
            |> text
            |> move (pixelCoordinates x 0)
            |> move (0, -10)
        xLabelLine x = traced (dashed darkGray) (segment (pixelCoordinates x minY) (pixelCoordinates x maxY))
        yLabelLine y = traced (dashed darkGray) (segment (pixelCoordinates minX y) (pixelCoordinates maxX y))
        xLabelPositions = labelPositions minX maxX
        yLabelPositions = labelPositions minY maxY
        yLabel y = y
            |> toString
            |> Text.fromString
            |> text
            |> move (pixelCoordinates 0 y)
            |> move (-20, 0)
        forms = List.concat 
            [ List.concat (List.map2 seriesForm colors g.points)
            , List.map xLabel xLabelPositions
            , List.map xLabelLine xLabelPositions
            , List.map yLabel yLabelPositions
            , List.map yLabelLine yLabelPositions
            , [ xAxis, yAxis ]
            ]
    in
    collage w h forms
