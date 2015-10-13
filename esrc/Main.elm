import Index exposing (..)

import StartApp.Simple exposing (start)
import Html exposing (Html)


main =
  start
    { model = Index.init
    , update = Index.update
    , view = Index.view
    }
