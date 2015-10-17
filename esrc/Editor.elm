module Editor (Model, init, Action, update, view) where

import Json.Decode as Json
import Task
import Http exposing (..)
import Maybe exposing (..)
import Effects exposing (Effects, Never)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (class, type', attribute, id, classList, href, rows, value)
import Html exposing (..)
import Html.Shorthand exposing (..)

alldice : String
alldice = "http://0.0.0.0:8081/roll"

-- MODEL
type alias Model =
    { program : String
    , programType : String
    , view : String
    , results : String
    }

init : (Model, Effects Action)
init =
    ( { program = "(roll 10 (dice 6))"
      , programType = "Scheme"
      , view = "Table"
      , results = ""
      }
    , Effects.none
    )


-- UPDATE
type Action
    = Submit
    | Results (Maybe String)
    | ChangeProg String
    | ChangeType String
    | ChangeView String

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Submit       -> (model, getDiceRoll model.program model.programType model.view)
        Results x    -> ({ model | results <- Maybe.withDefault "Error" x}, Effects.none)
        ChangeProg x -> ({ model | program <- x}, Effects.none)
        ChangeType x -> ({ model | programType <- x}, Effects.none)
        ChangeView x -> ({ model | view <- x}, Effects.none)


-- TODO: fancier type (Json and all)
getDiceRoll : String -> String -> String -> Effects Action
getDiceRoll program pType view =
    Http.get decodeResponse (encodeRequest program pType view)
        |> Task.toMaybe
        |> Task.map Results
        |> Effects.task


(=>) = (,)

encodeRequest : String -> String -> String -> String
encodeRequest program pType view =
    Http.url alldice
        [ "src" => program
        ]

decodeResponse : Json.Decoder String
decodeResponse =
    Json.at ["output"] Json.string


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div' {class = "container-fluid"} [ viewEditor address model, viewResult model ]


viewEditor : Signal.Address Action -> Model -> Html
viewEditor address model =
    div [id "diceInput"]
        [ form []
            [ div' {class="form-group"}
                [ label' {for="diceProgram", class="diceProgramLabel"} [ text "Dice Program" ]
                , textarea
                    [ id "diceProgram"
                    , class "form-control"
                    , type' "text"
                    , rows 10
                    , value model.program
                    , on "input" targetValue (Signal.message address << ChangeProg)
                    ] []
                ]
            , div' {class="form-inline"}
                [ button
                    [ type' "submit"
                    , class "btn btn-default"
                    , onClick address Submit
                    ]
                    [ text "Submit" ]
                , div' {class="form-group"}
                    [ label' {for="diceType", class="diceTypeLabel"} [ text "Program Type" ]
                    , select
                        [ id "diceType"
                        , class "form-control"
                        , on "input" targetValue (Signal.message address << ChangeType)
                        ]
                        [ option [] [ text "Scheme" ]
                        , option [] [ text "PCGen" ]
                        ]
                    ]
                , div' {class="form-group"}
                    [ label' {for="dataView", class="dataViewLabel"} [ text "View" ]
                    , select
                        [ id "dataView"
                        , class "form-control"
                        , on "input" targetValue (Signal.message address << ChangeType)
                        ]
                        [ option [] [ text "Table" ]
                        , option [] [ text "Graph" ]
                        ]
                    ]
                ]
            ]
        ]


viewResult : Model -> Html
viewResult model =
    div [id "diceResult", class "panel panel-default"]
        [ text model.results
        ]
