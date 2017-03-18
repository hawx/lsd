module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Collage
import Element
import Color
import Mouse
import Random
import Shapes exposing (Diamond, Star)
import Helpers exposing (within)
import Canvas exposing (..)

main =
    Html.program
        { init = (model, Random.generate AddStar randomPoint)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Model

type alias Model =
    { diamondStart : Maybe (Float, Float)
    , position : (Float, Float)
    , stars : List Star
    , diamonds : List Diamond
    }

model : Model
model =
    { diamondStart = Nothing
    , position = (0, 0)
    , stars = [ Star (50, 50) False
              , Star (100, 100) False
              , Star (90, 130) False
              , Star (60, 170) False
              , Star (170, 200) False
              , Star (300, 180) False

              , Star (50, 100) False
              ]
    , diamonds = []
    }

randomPoint : Random.Generator (Float, Float)
randomPoint =
    Random.pair (Random.float 0 gameWidth) (Random.float 0 gameHeight)

-- Update

type Msg = MouseClick Mouse.Position
         | MouseMove Mouse.Position
         | AddStar (Float, Float)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseClick position ->
            let
                selectedStar = clickedStar position model.stars
            in
                { model | diamondStart = Maybe.map .center selectedStar  } ! []

        MouseMove position ->
            { model | position = getLocalPosition position } ! []

        AddStar pos ->
            if tooClose pos model.stars then
                model ! [ Random.generate AddStar randomPoint ]
            else
                if List.length model.stars < totalStars then
                    { model | stars = Star pos False :: model.stars } ! [ Random.generate AddStar randomPoint ]
                else
                    { model | stars = Star pos False :: model.stars } ! []


tooClose : (Float, Float) -> List Star -> Bool
tooClose ((x, y) as pos) stars =
    let
        minDistance = 50
        margin = 10
    in
        List.any (\star -> within star.center minDistance pos) stars || x < margin || x > gameWidth - margin || y < margin || y > gameHeight - margin

clickedStar : Mouse.Position -> List Star -> Maybe Star
clickedStar clickPos stars =
    List.head <| List.filter (\star -> within star.center 8 (getLocalPosition clickPos)) stars

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
        [ Just <| List.map (Shapes.drawStar model.position) model.stars
        , Maybe.map List.singleton <| possibleDiamondAt model.diamondStart model.position
        ]
        |> List.concat
        |> Collage.collage gameHeight gameWidth

possibleDiamondAt : Maybe (Float, Float) -> (Float, Float) -> Maybe Collage.Form
possibleDiamondAt start end =
    Maybe.map (\s -> Shapes.drawDiamond { start = s, end = end }) start

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseClick
        , Mouse.moves MouseMove
        ]
