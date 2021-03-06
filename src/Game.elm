module Game exposing (init, view, update, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Collage
import Element
import Color
import Mouse
import Random
import Star exposing (Star)
import Diamond exposing (Diamond)
import Vector
import Canvas exposing (..)
import Players exposing (..)
import Time exposing (Time)

turnTime : Time
turnTime = 30

init : (Model, Cmd Msg)
init =
    (model, Random.generate AddStar randomPoint)

-- Model

type State = NotStarted | Playing | Skipped | Won Player | Draw

type alias Model =
    { diamondStart : Maybe (Float, Float)
    , position : (Float, Float)
    , stars : List Star
    , diamonds : List Diamond
    , playerA : Int
    , playerB : Int
    , currentPlayer : Player
    , timer : Time
    , state : State
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
    , timer = turnTime
    , state = NotStarted
    }

randomPoint : Random.Generator (Float, Float)
randomPoint =
    Random.pair (Random.float 0 gameWidth) (Random.float 0 gameHeight)

-- Update

type Msg = MouseClick Mouse.Position
         | MouseMove Mouse.Position
         | AddStar (Float, Float)
         | Tick Time
         | StartGame

start : Model
start =
    { model | state = Playing }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartGame ->
            start ! [ Random.generate AddStar randomPoint ]

        MouseClick position ->
            let
                selectedStar = Maybe.map .center <| clickedStar position model.stars
            in
                case (model.diamondStart, selectedStar) of
                    (Just diamondStart, Just selectedStar) ->
                        if List.any .overlaps <| overlappedDiamonds (Diamond diamondStart selectedStar False model.currentPlayer) model.diamonds then
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
                    let
                        selectedStar =
                            Maybe.map .center <| clickedStar position model.stars

                        drawDiamond pos =
                            { model
                                | position = pos
                                , diamonds = overlappedDiamonds (Diamond diamondStart pos False model.currentPlayer) model.diamonds
                            }
                    in
                        drawDiamond (Maybe.withDefault (getLocalPosition position) selectedStar) ! []

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

        Tick time ->
            if model.timer == 0 then
                if model.state == Skipped then
                    { model | state = winner model.playerA model.playerB } ! []
                else
                    endTurn 0 { model | diamondStart = Nothing, state = Skipped } ! []
            else
                { model | timer = model.timer - 1 } ! []

winner : Int -> Int -> State
winner playerA playerB =
    if playerA > playerB then
        Won A
    else
        if playerB > playerA then
            Won B
        else
            Draw


endTurn : Int -> Model -> Model
endTurn score model =
    case model.currentPlayer of
        A ->
            { model
                | playerA = model.playerA + score
                , currentPlayer = B
                , timer = turnTime
            }
        B ->
            { model
                | playerB = model.playerB + score
                , currentPlayer = A
                , timer = turnTime
            }

tooClose : (Float, Float) -> List Star -> Bool
tooClose ((x, y) as pos) stars =
    let
        minDistance = 50
        margin = 10
    in
        List.any (\star -> Vector.within star.center minDistance pos) stars
            || x < margin || x > gameWidth - margin || y < margin || y > gameHeight - margin

clickedStar : Mouse.Position -> List Star -> Maybe Star
clickedStar clickPos stars =
    List.head <| List.filter (\star -> Vector.within star.center Star.hoverRadius (getLocalPosition clickPos)) stars

overlappedDiamonds : Diamond -> List Diamond -> List Diamond
overlappedDiamonds diamond =
    List.map (\x -> { x | overlaps = Diamond.overlap diamond x })

-- View

view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "game" ] <|
              List.concat [ overlay model, [ game model ] ]
        , scores model
        ]

overlay : Model -> List (Html Msg)
overlay model =
    case model.state of
        NotStarted ->
            [ Html.div [ Attr.class "overlay", Event.onClick StartGame ]
                  [ Html.h1 [] [ Html.text "Click to play" ] ]
            ]

        Playing ->
            []

        Skipped ->
            []

        Won player ->
            case player of
                A ->
                    [ Html.div [ Attr.class "overlay", Event.onClick StartGame ]
                          [ Html.h1 [] [ Html.text "Player A Won" ]
                          , Html.h2 [] [ Html.text "Click to play again" ]
                          ]
                    ]
                B ->
                    [ Html.div [ Attr.class "overlay", Event.onClick StartGame ]
                          [ Html.h1 [] [ Html.text "Player B Won" ]
                          , Html.h2 [] [ Html.text "Click to play again" ]
                          ]
                    ]

        Draw ->
            [ Html.div [ Attr.class "overlay", Event.onClick StartGame ]
                  [ Html.h1 [] [ Html.text "It was a Draw" ]
                  , Html.h2 [] [ Html.text "Click to play again" ]
                  ]
            ]


scores : Model -> Html Msg
scores model =
    Html.div [ Attr.class "scores" ]
        [ Html.div [ Attr.class "score" ]
              [ Html.h1 [] [ Html.text "Turn Time" ]
              , Html.h2 [] [ Html.text (toString model.timer) ]
              ]
        , Html.div [ Attr.class "score"
                   , Attr.classList [ ("active", model.currentPlayer == A && isPlaying model.state) ]
                   ]
            [ Html.h1 [] [ Html.text "Player A" ]
            , Html.h2 [] [ Html.text (toString model.playerA) ]
            ]
        , Html.div [ Attr.class "score"
                   , Attr.classList [ ("active", model.currentPlayer == B && isPlaying model.state) ]
                   ]
            [ Html.h1 [] [ Html.text "Player B" ]
            , Html.h2 [] [ Html.text (toString model.playerB) ]
            ]
        ]

isPlaying : State -> Bool
isPlaying state =
    state == Playing || state == Skipped

game : Model -> Html msg
game model =
    List.filterMap identity
        [ Just <| [ background ]
        , Just <| List.map (Star.draw model.position) model.stars
        , Just <| List.map Diamond.draw model.diamonds
        , Maybe.map List.singleton <| possibleDiamondAt model.currentPlayer model.diamondStart model.position
        ]
        |> List.concat
        |> Collage.collage gameHeight gameWidth
        |> Element.toHtml

background : Collage.Form
background =
    Collage.gradient
        (Color.linear (0, gameHeight / 2) (0, -gameHeight / 2) [ (0.6, Color.hsl 0 0 0), (1, Color.hsl (degrees 240) 1 0.1)])
        (Collage.rect gameHeight gameWidth)

possibleDiamondAt : Player -> Maybe (Float, Float) -> (Float, Float) -> Maybe Collage.Form
possibleDiamondAt currentPlayer start end =
    Maybe.map (\s -> Diamond.draw { start = s
                                  , end = end
                                  , overlaps = False
                                  , player = currentPlayer }) start

subscriptions : Model -> Sub Msg
subscriptions model =
    if isPlaying model.state then
        Sub.batch
            [ Mouse.clicks MouseClick
            , Mouse.moves MouseMove
            , Time.every Time.second Tick
            ]
    else
        Sub.none
