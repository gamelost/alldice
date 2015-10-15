module Editor (Model, init, Action, update, view) where

import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type', attribute, id, classList, href, rows)
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
    div' {class = "container-fluid"} [ viewEditor, viewResult ]


viewEditor : Html
viewEditor =
    div [id "diceInput"]
        [ form []
            [ div' {class="form-group"}
                [ label' {for="diceProgram", class="diceProgramLabel"} [ text "Dice Program" ]
                , textarea [id "diceProgram", class "form-control", type' "text", rows 10] []
                ]
            , div' {class="form-inline"}
                [ button [type' "submit", class "btn btn-default"] [ text "Submit" ]
                , div' {class="form-group"}
                    [ label' {for="diceType", class="diceTypeLabel"} [ text "Program Type" ]
                    , select [id "diceType", class "form-control"]
                        [ option [] [ text "Scheme" ]
                        , option [] [ text "PCGen" ]
                        ]
                    ]
                , div' {class="form-group"}
                    [ label' {for="dataView", class="dataViewLabel"} [ text "View" ]
                    , select [id "dataView", class "form-control"]
                        [ option [] [ text "Table" ]
                        , option [] [ text "Graph" ]
                        ]
                    ]
                ]
            ]
        ]


viewResult : Html
viewResult =
    div [id "diceResult", class "panel panel-default"]
        [ text "Foobar"
        ]
