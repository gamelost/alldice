module Navbar (Model, init, update, view) where

import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type', attribute, id, classList, href)
import Html exposing (..)
import Html.Shorthand exposing (..)


-- MODEL
type alias Model a =
    { brand : (String, a)
    , pages : List (String, a)
    , active : a
    }

type alias ID = Int

-- TODO: figure out a good way deal with the active tab (even if there is none, probably Maybe here)
init : (String, a) -> List (String, a) -> a -> Model a
init brand pages active =
    case pages of
        [] -> { brand = brand, pages = [], active = active }
        xs -> { brand = brand, pages = xs, active = active }


-- UPDATE
update : Model a -> a -> Model a
update model x = { model | active <- x }


-- VIEW
view : Signal.Address a -> Model a -> Html
view address model =
    nav' { class = "navbar navbar-default navbar-static-top" }
        [ div' { class = "container-fluid" }
            [ div' { class = "navbar-header" }
                -- TODO: tweak when it collapses cos it collapses too soon at the moment
                -- Customize the @grid-float-breakpoint variable or add your own media query.
                [ button hamburgerButtonAttrs hamburger
                , brand address model
                ]
            , div [ class "navbar-collapse collapse", id "navbar" ]
                (List.map (viewNavElement address model.active) model.pages)
            ]
        ]

brand : Signal.Address a -> Model a -> Html
brand address model =
    a [ onClick address (snd model.brand), class "navbar-brand", href ("#" ++ (fst model.brand)) ] [ text (fst model.brand) ]


viewNavElement : Signal.Address a -> a -> (String, a) -> Html
viewNavElement address activeID (name, x) =
    li
        [ classList [ ("active", x == activeID) ] ]
        [ a [ onClick address x, href ("#" ++ name) ] [ text name ] ]


hamburgerButtonAttrs : List Attribute
hamburgerButtonAttrs =
    [ class "navbar-toggle collapsed"
    , type' "button"
    , attribute "data-toggle" "collapse"
    , attribute "data-target" "#navbar"
    , attribute "aria-expanded" "false"
    , attribute "aria-controls" "navbar"
    ]

hamburger : List Html
hamburger =
    [ span' { class = "sr-only" } [ text "Toggle navigation" ]
    , span' { class = "icon-bar" } []
    , span' { class = "icon-bar" } []
    , span' { class = "icon-bar" } []
    ]
