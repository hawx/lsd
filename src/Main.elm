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
    , stars = []
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
                selectedStar = Maybe.map .center <| clickedStar position model.stars
            in
                case (model.diamondStart, selectedStar) of
                    (Just diamondStart, Just selectedStar) ->
                        if List.any .overlaps model.diamonds then
                            model ! []
                        else
                            { model
                                | diamondStart = Nothing
                                , diamonds = Diamond diamondStart selectedStar False :: model.diamonds
                            } ! []

                    (Just _, Nothing) ->
                        { model
                            | diamondStart = Nothing
                            , diamonds = List.map (\x -> { x | overlaps = False }) model.diamonds
                        } ! []

                    (Nothing, Just _) ->
                        { model | diamondStart = selectedStar } ! []

                    (Nothing, Nothing) ->
                        model ! []

        MouseMove position ->
            case model.diamondStart of
                Just diamondStart ->
                    { model
                        | position = getLocalPosition position
                        , diamonds = overlappedDiamonds (Diamond diamondStart (getLocalPosition position) False) model.diamonds
                    } ! []

                Nothing ->
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

overlappedDiamonds : Diamond -> List Diamond -> List Diamond
overlappedDiamonds diamond =
    List.map (\x -> { x | overlaps = Shapes.overlap diamond x })

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
        , Just <| List.map Shapes.drawDiamond model.diamonds
        , Maybe.map List.singleton <| possibleDiamondAt model.diamondStart model.position
        ]
        |> List.concat
        |> Collage.collage gameHeight gameWidth

possibleDiamondAt : Maybe (Float, Float) -> (Float, Float) -> Maybe Collage.Form
possibleDiamondAt start end =
    Maybe.map (\s -> Shapes.drawDiamond { start = s, end = end, overlaps = False }) start

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseClick
        , Mouse.moves MouseMove
        ]
