module Static (viewFooter) where

import Html exposing (..)
import Html.Shorthand exposing (..)


viewFooter : Html
viewFooter =
    footer' { class = "footer" }
        [ div' { class = "container-fluid" }
            [ p' { class = "text-muted" }
                [ text "Copyright © Anja Berens"
                ]
            ]
        ]
