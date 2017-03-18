module Vector exposing (..)

type alias Point =
    (Float, Float)

type alias Vector =
    (Point, Point)

rotate : Float -> Point -> Point
rotate theta (x, y) =
    ( x * cos theta - y * sin theta
    , x * sin theta + y * cos theta
    )

move : (Float, Float) -> Point -> Point
move (dx, dy) (x, y) =
    ( x + dx
    , y + dy
    )

intersect : Vector -> Vector -> Bool
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

onSegment : Point -> Point -> Point -> Bool
onSegment (px, py) (qx, qy) (rx, ry) =
    qx <= max px rx && qx >= min px rx
        && qy <= max py ry && qy >= min py ry

orientation : Point -> Point -> Point -> Int
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

angle : Point -> Point -> Float
angle (startX, startY) (endX, endY) =
    atan2 (endY - startY) (endX - startX)

distance : Point -> Point -> Float
distance (startX, startY) (endX, endY) =
    sqrt ((endX - startX)^2 + (endY - startY)^2)

within : Point -> Float -> Point -> Bool
within (targetX, targetY) radius (actualX, actualY) =
    targetX - radius <= actualX && actualX <= targetX + radius
        && targetY - radius <= actualY && actualY <= targetY + radius
