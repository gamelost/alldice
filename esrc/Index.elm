module Index (Model, init, Action, update, view) where

import Navbar
import Static
import Editor

import Effects exposing (Effects)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type', attribute, id, classList, href)
import Html exposing (..)
import Html.Shorthand exposing (..)


-- MODEL
type Page = Dice | Docs | About

type alias Model =
    { activePage : Page
    , navBar : Navbar.Model Action
    , editor : Editor.Model
    }

init : (Model, Effects Action)
init =
    let
        (editor, action) = Editor.init
    in
        ( { activePage = Dice
          , navBar = Navbar.init
              ("AllDice", (To Dice))
              [ ("Dice", (To Dice))
              , ("Docs", (To Docs))
              , ("About", (To About))
              ]
              (To Dice)
          , editor = editor
          }
        , Effects.map Edit action
        )


-- UPDATE
type Action = To Page
            | Edit Editor.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        (To page)  -> ( { model | activePage <- page, navBar <- Navbar.update model.navBar (To page) }
                      , Effects.none
                      )
        (Edit act) ->
            let
                (editor, acted) = Editor.update act model.editor
            in
                ( { model | editor <- editor }
                , Effects.map Edit acted
                )


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div_
        [ Navbar.view address model.navBar
        , case model.activePage of
            Dice  -> Editor.view (Signal.forwardTo address Edit) model.editor
            Docs  -> Static.viewDocumentation
            About -> Static.viewAbout
        , Static.viewFooter
        ]
