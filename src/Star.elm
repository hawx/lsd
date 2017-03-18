module Star exposing ( Star
                     , draw
                     , hoverRadius
                     )

import Collage
import Color
import Vector
import Canvas exposing (absoluteToCanvas)


type alias Star =
    { center : (Float, Float)
    , selected : Bool
    }

starRadius = 5
hoverRadius = 8

draw : (Float, Float) -> Star -> Collage.Form
draw mousePos star =
    let
        isOver =
            Vector.within star.center hoverRadius mousePos
    in
        Collage.circle starRadius
            |> Collage.filled (starColour isOver star.selected)
            |> Collage.move (absoluteToCanvas star.center)


starColour : Bool -> Bool -> Color.Color
starColour isOver isSelected =
    if isSelected then
        Color.hsl (degrees 55) 1 0.5
    else
        if isOver then
            Color.hsl (degrees 55) 1 0.3
        else
            Color.hsl (degrees 250) 1 0.5
