import Index exposing (..)

import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (href, rel, src, type')


main =
  start
    { model = Index.init
    , update = Index.update
    , view = customView
    }

http : String
http = "http://0.0.0.0:8080/"

customView : Signal.Address Index.Action -> Index.Model -> Html
customView action model =
    div []
        -- Load up all of the secondary stuff needed
        [ node "link" [href (http ++ "style.css"), rel "stylesheet"] []
--        , node "script" [src (http ++ "jquery.min.js")] []
--        , node "script" [src (http ++ "bootstrap.min.js")] []
        , node "script" [src (http ++ "custom.js")] []

        -- Actually startup the app
        , Index.view action model
        ]
