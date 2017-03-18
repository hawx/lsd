module Star exposing ( Star
                     , draw
                     )

import Collage
import Color
import Vector
import Canvas exposing (absoluteToCanvas)


type alias Star =
    { center : (Float, Float)
    , selected : Bool
    }


draw : (Float, Float) -> Star -> Collage.Form
draw mousePos star =
    let
        isOver =
            Vector.within star.center 8 mousePos

        radius =
            5
    in
        Collage.circle radius
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
