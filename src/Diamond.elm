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
    points diamond
        |> Collage.polygon
        |> Collage.outlined (Collage.solid (diamondColour overlaps player))


points : Diamond -> List (Float, Float)
points { start, end } =
    [ (0, 0), (0.7, 0.25), (1, 0), (0.7, -0.25) ]
        |> List.map (Vector.scale (Vector.distance start end))
        |> List.map (Vector.rotate -(Vector.angle start end))
        |> List.map (Vector.move (absoluteToCanvas start))


edges : Diamond -> List ((Float, Float), (Float, Float))
edges diamond =
    case points diamond of
        [p, q, r, s] ->
            [(p, q), (q, r), (r, s), (s, p)]

        _ ->
            []


overlap : Diamond -> Diamond -> Bool
overlap a b =
    any2 (\x y -> Vector.toInt x == Vector.toInt y) (points a) (points b)
        || any2 (Vector.intersect) (edges a) (edges b)


any2 : (a -> b -> Bool) -> List a -> List b -> Bool
any2 f xs ys =
    List.any (\x -> List.any (\y -> f x y) ys) xs


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
