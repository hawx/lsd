module Main exposing (main)

import Html
import Game

main =
    Html.program
        { init = Game.init
        , view = Game.view
        , update = Game.update
        , subscriptions = Game.subscriptions
        }
