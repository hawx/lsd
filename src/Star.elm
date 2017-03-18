module Star exposing ( Star
                     , draw
                     )

import Collage
import Color
import Helpers exposing (distance, angle, within)
import Canvas exposing (absoluteToCanvas)


type alias Star =
    { center : (Float, Float)
    , selected : Bool
    }


draw : (Float, Float) -> Star -> Collage.Form
draw mousePos star =
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
