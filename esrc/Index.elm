module Index (Model, init, Action, update, view) where

import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type', attribute, id, classList, href)
import Html exposing (..)
import Html.Shorthand exposing (..)

-- MODEL
type Model = Dice | Docs | About

init : Model
init = Dice

buttonList : List (Model, String)
buttonList =
    [ (Dice, "Dice")
    , (Docs, "Docs")
    , (About, "About")
    ]

-- UPDATE
type Action = To Model

update : Action -> Model -> Model
update (To action) _ = action


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model = viewNavbar address model


viewNavbar : Signal.Address Action -> Model -> Html
viewNavbar address model =
    nav' { class = "navbar navbar-default navbar-static-top" }
        [ div' { class = "container-fluid" }
            [ div' { class = "navbar-header" }
                -- TODO: tweak when it collapses cos it collapses too soon at the moment
                -- Customize the @grid-float-breakpoint variable or add your own media query.
                [ button
                    [ class "navbar-toggle collapsed"
                    , type' "button"
                    , attribute "data-toggle" "collapse"
                    , attribute "data-target" "#navbar"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-controls" "navbar"
                    ]
                    [ span' { class = "sr-only" } [ text "Toggle navigation" ]
                    , span' { class = "icon-bar" } []
                    , span' { class = "icon-bar" } []
                    , span' { class = "icon-bar" } []
                    ]
                , a [ onClick address (To Dice), class "navbar-brand", href ("#" ++ (toString Dice)) ] [ text "AllDice" ]
                ]
            , div [ class "navbar-collapse collapse", id "navbar" ]
                (List.map (viewButton address model) buttonList)
            ]
        ]

viewButton : Signal.Address Action -> Model -> (Model, String) -> Html
viewButton address activeModel (model, title) =
    li
        [ classList [ ("active", model == activeModel) ] ]
        [ a [ onClick address (To model), href ("#" ++ (toString model)) ] [ text title ] ]

viewFooter : Html
viewFooter =
    footer' { class = "footer" }
        [ div' { class = "container-fluid" }
            [ p' { class = "text-muted" }
                [ text "Copyright © Anja Berens"
                ]
            ]
        ]
