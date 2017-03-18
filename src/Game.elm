module Game exposing (init, view, update, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attr
import Collage
import Element
import Color
import Mouse
import Random
import Star exposing (Star)
import Diamond exposing (Diamond)
import Helpers exposing (within)
import Canvas exposing (..)
import Players exposing (..)

init : (Model, Cmd Msg)
init =
    (model, Random.generate AddStar randomPoint)

-- Model

type alias Model =
    { diamondStart : Maybe (Float, Float)
    , position : (Float, Float)
    , stars : List Star
    , diamonds : List Diamond
    , playerA : Int
    , playerB : Int
    , currentPlayer : Player
    }

model : Model
model =
    { diamondStart = Nothing
    , position = (0, 0)
    , stars = []
    , diamonds = []
    , playerA = 0
    , playerB = 0
    , currentPlayer = A
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
                            endTurn 1 { model
                                | diamondStart = Nothing
                                , diamonds = Diamond diamondStart selectedStar False model.currentPlayer :: model.diamonds
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
                        , diamonds = overlappedDiamonds (Diamond diamondStart (getLocalPosition position) False model.currentPlayer) model.diamonds
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


endTurn : Int -> Model -> Model
endTurn score model =
    case model.currentPlayer of
        A ->
            { model | playerA = model.playerA + score, currentPlayer = B }
        B ->
            { model | playerB = model.playerB + score, currentPlayer = A }

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
    List.map (\x -> { x | overlaps = Diamond.overlap diamond x })

-- View

view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "game" ]
              [ Element.toHtml (game model) ]
        , Html.div [ Attr.class "scores" ]
            [ Html.div [ Attr.class "a" ]
                  [ Html.h1 [] [ Html.text "Player A" ]
                  , Html.h2 [] [ Html.text (toString model.playerA) ]
                  ]
            , Html.div [ Attr.class "b" ]
                [ Html.h1 [] [ Html.text "Player B" ]
                , Html.h2 [] [ Html.text (toString model.playerB) ]
                ]
            ]
        ]

game : Model -> Element.Element
game model =
    List.filterMap identity
        [ Just <| [ background ]
        , Just <| List.map (Star.draw model.position) model.stars
        , Just <| List.map Diamond.draw model.diamonds
        , Maybe.map List.singleton <| possibleDiamondAt model.currentPlayer model.diamondStart model.position
        ]
        |> List.concat
        |> Collage.collage gameHeight gameWidth

background : Collage.Form
background =
    Collage.gradient (Color.linear
                          (0, gameHeight / 2)
                          (0, -gameHeight / 2)
                          [ (0.6, Color.hsl 0 0 0)
                          , (1, Color.hsl (degrees 240) 1 0.1)
                          ]
                     ) (Collage.rect gameHeight gameWidth)

possibleDiamondAt : Player -> Maybe (Float, Float) -> (Float, Float) -> Maybe Collage.Form
possibleDiamondAt currentPlayer start end =
    Maybe.map (\s -> Diamond.draw { start = s
                                  , end = end
                                  , overlaps = False
                                  , player = currentPlayer }) start

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseClick
        , Mouse.moves MouseMove
        ]
