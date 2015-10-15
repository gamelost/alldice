module Editor (Model, init, Action, update, view) where

import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type', attribute, id, classList, href)
import Html exposing (..)
import Html.Shorthand exposing (..)


-- MODEL
type alias Model =
    { test : String
    }

init : Model
init = { test = "hi" }


-- UPDATE
type Action = Test

update : Action -> Model -> Model
update action model = model


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    nav' {class = "navbar navbar-default navbar-static-top"}
        [ text model.test
        ]
