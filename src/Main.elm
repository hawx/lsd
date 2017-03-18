module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Collage
import Element
import Color
import Mouse

main =
    Html.program
        { init = (model, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Model

type alias Model =
    { click : Maybe Mouse.Position
    , position : Mouse.Position
    , stars : List (Float, Float)
    }

model : Model
model =
    { click = Nothing
    , position = { x = 0, y = 0 }
    , stars = [ (50, 50)
              , (100, 100)
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
            { model | click = Just position } ! []

        MouseUp position ->
            { model | click = Nothing } ! []

        MouseMove position ->
            { model | position = position } ! []

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
    List.map starAt model.stars
        |> Collage.collage 700 700

starAt : (Float, Float) -> Collage.Form
starAt pos =
    Collage.move pos star

star : Collage.Form
star =
    Collage.filled (Color.hsl 350 100 100) (Collage.circle 5)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        ]
