module Diamond exposing ( Diamond
                        , draw
                        , overlap
                        )

import Collage
import Color
import Helpers exposing (distance, angle, within)
import Canvas exposing (absoluteToCanvas)
import Players exposing (Player)

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
            distance start end
    in
        [ (0, 0)
        , (scale * 0.7, scale * 0.25)
        , (scale, 0)
        , (scale * 0.7, -scale * 0.25)
        ]
        |> List.map (rotate -(angle start end))
        |> List.map (move (absoluteToCanvas start))

rotate : Float -> (Float, Float) -> (Float, Float)
rotate theta (x, y) =
    (x * cos theta - y * sin theta, x * sin theta + y * cos theta)

move : (Float, Float) -> (Float, Float) -> (Float, Float)
move (dx, dy) (x, y) =
    (dx + x, dy + y)

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
        List.any (\x -> List.any (intersect x) aLines) bLines

intersect : ((Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float)) -> Bool
intersect (p1, q1) (p2, q2) =
    let
        o1 = orientation p1 q1 p2
        o2 = orientation p1 q1 q2
        o3 = orientation p2 q2 p1
        o4 = orientation p2 q2 q1
    in
        -- general case
        o1 /= o2 && o3 /= o4
            -- special cases
            || o1 == 0 && onSegment p1 p2 q1
            || o2 == 0 && onSegment p1 q2 q1
            || o3 == 0 && onSegment p2 p1 q2
            || o4 == 0 && onSegment p2 q1 q2

onSegment : (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
onSegment (px, py) (qx, qy) (rx, ry) =
    qx <= max px rx && qx >= min px rx
        && qy <= max py ry && qy >= min py ry

orientation : (Float, Float) -> (Float, Float) -> (Float, Float) -> Int
orientation (px, py) (qx, qy) (rx, ry) =
    let
        val = (qy - py) * (rx - qx) -
              (qx - px) * (ry - qy)
    in
        if val == 0 then
            0
        else
            if val > 0 then
                1
            else
                2

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
