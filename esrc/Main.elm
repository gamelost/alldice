import Index exposing (..)

import Task
import Effects exposing (Never)
import StartApp
import Html exposing (..)
import Html.Attributes exposing (href, rel, src, type')

asset : String
asset = "http://0.0.0.0:8001/"

app =
    StartApp.start
        { init = Index.init
        , update = Index.update
        , view = customView
        , inputs = []
        }

main = app.html


port tasks : Signal (Task.Task Never ())
port tasks = app.tasks


customView : Signal.Address Index.Action -> Index.Model -> Html
customView action model =
    div []
        -- Load up all of the secondary stuff needed
        [ node "link" [href (asset ++ "style.css"), rel "stylesheet"] []

        -- Actually startup the app
        , Index.view action model
        ]
