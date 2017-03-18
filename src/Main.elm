module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Collage
import Element
import Color
import Mouse
import Debug

gameMargin : Float
gameMargin = 41

gameHeight : number
gameHeight = 700

gameWidth : number
gameWidth = 700

main =
    Html.program
        { init = (model, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Model

type alias Model =
    { click : Maybe (Float, Float)
    , position : (Float, Float)
    , stars : List (Float, Float)
    }

model : Model
model =
    { click = Nothing
    , position = (0, 0)
    , stars = [ (50, 50)
              , (100, 100)
              , (50, 100)
              ]
    }

-- Update

type Msg = MouseDown Mouse.Position
         | MouseUp Mouse.Position
         | MouseMove Mouse.Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseDown position ->
            { model | click = Just (getLocalPosition position) } ! []

        MouseUp position ->
            { model | click = Nothing } ! []

        MouseMove position ->
            { model | position = getLocalPosition position } ! []

getLocalPosition : Mouse.Position -> (Float, Float)
getLocalPosition { x, y } =
    (toFloat x - gameMargin, toFloat y - gameMargin)

absoluteToCanvas : (Float, Float) -> (Float, Float)
absoluteToCanvas (x, y) =
    (x - (gameWidth / 2), (gameHeight / 2) - y)

-- View

view : Model -> Html Msg
view model =
    Html.div [ Attr.class "game" ]
        [ Element.toHtml (game model)
        , Html.text (toString model.click)
        , Html.text (toString model.position)
        ]

game : Model -> Element.Element
game model =
    List.map (starAt model.position) model.stars
        |> Collage.collage gameHeight gameWidth

starAt : (Float, Float) -> (Float, Float) -> Collage.Form
starAt mousePos pos =
    Collage.move (absoluteToCanvas pos) (star (within pos 5 mousePos))

within : (Float, Float) -> Float -> (Float, Float) -> Bool
within (targetX, targetY) radius (actualX, actualY) =
    targetX - radius <= actualX && actualX <= targetX + radius
        && targetY - radius <= actualY && actualY <= targetY + radius

star : Bool -> Collage.Form
star isOver =
    Collage.filled (starColour isOver) (Collage.circle 5)

starColour : Bool -> Color.Color
starColour isOver =
    if isOver then Color.hsl 70 100 100 else Color.hsl 350 100 50


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        ]
