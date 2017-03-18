module Canvas exposing (..)

import Mouse


gameMargin : Float
gameMargin = 41


gameHeight : number
gameHeight = 700


gameWidth : number
gameWidth = 700


totalStars : Int
totalStars = 30


getLocalPosition : Mouse.Position -> (Float, Float)
getLocalPosition { x, y } =
    (toFloat x - gameMargin, toFloat y - gameMargin)


absoluteToCanvas : (Float, Float) -> (Float, Float)
absoluteToCanvas (x, y) =
    (x - (gameWidth / 2), (gameHeight / 2) - y)
