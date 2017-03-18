module Shapes exposing ( Diamond
                       , drawDiamond
                       , Star
                       , drawStar
                       )

import Collage
import Color
import Helpers exposing (distance, angle, within)
import Canvas exposing (absoluteToCanvas)

type alias Diamond =
    { start : (Float, Float)
    , end : (Float, Float)
    }


drawDiamond : Diamond -> Collage.Form
drawDiamond { start, end } =
    let
        scale =
            distance start end

        base =
            Collage.polygon
                [ (0, 0)
                , (scale * 0.7, scale * 0.25)
                , (scale, 0)
                , (scale * 0.7, -scale * 0.25)
                ]
    in
        base
            |> Collage.outlined (Collage.solid (Color.hsl (degrees 350) 1 0.5))
            |> Collage.move (absoluteToCanvas start)
            |> Collage.rotate -(angle start end)


type alias Star =
    { center : (Float, Float)
    , selected : Bool
    }


drawStar : (Float, Float) -> Star -> Collage.Form
drawStar mousePos star =
    let
        isOver =
            within star.center 8 mousePos

        radius =
            5

        center (x, y) =
            (x - radius / 2, y - radius / 2)
    in
        Collage.circle radius
            |> Collage.filled (starColour isOver star.selected)
            |> Collage.move (center (absoluteToCanvas star.center))


starColour : Bool -> Bool -> Color.Color
starColour isOver isSelected =
    if isSelected then
        Color.hsl (degrees 350) 1 0.5
    else
        if isOver then
            Color.hsl (degrees 350) 1 0.3
        else
            Color.hsl (degrees 250) 1 0.5
