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
    { diamondStart : Maybe (Float, Float)
    , position : (Float, Float)
    , stars : List Star
    }

type alias Star =
    { position : (Float, Float)
    , selected : Bool
    }

model : Model
model =
    { diamondStart = Nothing
    , position = (0, 0)
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
            { model | diamondStart = Maybe.map .position <| clickedStar position model.stars } ! []

        MouseMove position ->
            { model | position = getLocalPosition position } ! []


clickedStar : Mouse.Position -> List Star -> Maybe Star
clickedStar clickPos stars =
    List.head <| List.filter (\star -> within star.position 8 (getLocalPosition clickPos)) stars

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
    List.filterMap identity
        [ Just <| List.map (starAt model.position) model.stars
        , Maybe.map List.singleton <| possibleDiamondAt model.diamondStart model.position
        ]
        |> List.concat
        |> Collage.collage gameHeight gameWidth

possibleDiamondAt : Maybe (Float, Float) -> (Float, Float) -> Maybe Collage.Form
possibleDiamondAt start end =
    Maybe.map (\s -> diamond s end) start

diamond : (Float, Float) -> (Float, Float) -> Collage.Form
diamond (startPosX, startPosY) (endPosX, endPosY) =
    let
        scale = sqrt (((endPosX - startPosX)^2) + ((endPosY - startPosY)^2))
        angle = atan2 (endPosY - startPosY) (endPosX - startPosX)
    in
        Collage.polygon
            [ (0, 0)
            , (scale * 0.7, scale * 0.25)
            , (scale, 0)
            , (scale * 0.7, -scale * 0.25)
            ]
            |> Collage.outlined (Collage.solid (Color.hsl (degrees 350) 1 0.5))
            |> Collage.move (absoluteToCanvas (startPosX, startPosY))
            |> Collage.rotate -angle

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
