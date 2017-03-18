module Helpers exposing (..)

distance : (Float, Float) -> (Float, Float) -> Float
distance (startX, startY) (endX, endY) =
    sqrt ((endX - startX)^2 + (endY - startY)^2)

angle : (Float, Float) -> (Float, Float) -> Float
angle (startX, startY) (endX, endY) =
    atan2 (endY - startY) (endX - startX)

within : (Float, Float) -> Float -> (Float, Float) -> Bool
within (targetX, targetY) radius (actualX, actualY) =
    targetX - radius <= actualX && actualX <= targetX + radius
        && targetY - radius <= actualY && actualY <= targetY + radius
