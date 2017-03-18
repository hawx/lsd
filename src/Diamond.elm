module Diamond exposing ( Diamond
                        , draw
                        , overlap
                        )

import Collage
import Color
import Canvas exposing (absoluteToCanvas)
import Players exposing (Player)
import Vector

type alias Diamond =
    { start : (Float, Float)
    , end : (Float, Float)
    , overlaps : Bool
    , player : Player
    }


draw : Diamond -> Collage.Form
draw ({ start, end, overlaps, player } as diamond) =
    diamondPoints diamond
        |> Collage.polygon
        |> Collage.outlined (Collage.solid (diamondColour overlaps player))

diamondPoints : Diamond -> List (Float, Float)
diamondPoints { start, end } =
    let
        scale =
            Vector.distance start end
    in
        [ (0, 0)
        , (scale * 0.7, scale * 0.25)
        , (scale, 0)
        , (scale * 0.7, -scale * 0.25)
        ]
        |> List.map (Vector.rotate -(Vector.angle start end))
        |> List.map (Vector.move (absoluteToCanvas start))

overlap : Diamond -> Diamond -> Bool
overlap a b =
    let
        lines pts =
            case pts of
                [p, q, r, s] ->
                    [(p, q), (q, r), (r, s), (s, p)]
                _ ->
                    []

        aLines = lines (diamondPoints a)
        bLines = lines (diamondPoints b)
    in
        List.any (\x -> List.any (Vector.intersect x) aLines) bLines

diamondColour : Bool -> Player -> Color.Color
diamondColour isOverlap player =
    if isOverlap then
        Color.hsl (degrees 0) 1 1
    else
        case player of
            Players.A ->
                Color.hsl (degrees 349) 1 0.5
            Players.B ->
                Color.hsl (degrees 55) 1 0.5
