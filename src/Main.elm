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
    { position : (Float, Float)
    , stars : List Star
    }

type alias Star =
    { position : (Float, Float)
    , selected : Bool
    }

model : Model
model =
    { position = (0, 0)
    , stars = [ Star (50, 50) False
              , Star (100, 100) False
              , Star (50, 100) False
              ]
    }

-- Update

type Msg = MouseClick Mouse.Position
         | MouseMove Mouse.Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseClick position ->
            { model | stars = selectedStars position model.stars } ! []

        MouseMove position ->
            { model | position = getLocalPosition position } ! []


selectedStars : Mouse.Position -> List Star -> List Star
selectedStars clickPos stars =
    List.map (\star ->
                  if within star.position 8 (getLocalPosition clickPos) then
                      { star | selected = not star.selected }
                  else
                      star
             ) stars

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
        , Html.text (toString model.position)
        ]

game : Model -> Element.Element
game model =
    List.map (starAt model.position) model.stars
        |> Collage.collage gameHeight gameWidth

starAt : (Float, Float) -> Star -> Collage.Form
starAt mousePos star =
    Collage.move (absoluteToCanvas star.position) (starShape (within star.position 8 mousePos) star.selected)

within : (Float, Float) -> Float -> (Float, Float) -> Bool
within (targetX, targetY) radius (actualX, actualY) =
    targetX - radius <= actualX && actualX <= targetX + radius
        && targetY - radius <= actualY && actualY <= targetY + radius

starShape : Bool -> Bool -> Collage.Form
starShape isOver isSelected =
    Collage.filled (starColour isOver isSelected) (Collage.circle 5)

starColour : Bool -> Bool -> Color.Color
starColour isOver isSelected =
    if isSelected then
        Color.hsl (degrees 350) 1 0.5
    else
        if isOver then
            Color.hsl (degrees 350) 1 0.3
        else
            Color.hsl (degrees 250) 1 0.5


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseClick
        , Mouse.moves MouseMove
        ]
