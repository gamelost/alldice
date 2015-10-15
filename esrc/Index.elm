module Index (Model, init, Action, update, view) where

import Navbar
import Static
import Editor

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

init : Model
init =
    { activePage = Dice
    , navBar = Navbar.init
        ("AllDice", (To Dice))
        [ ("Dice", (To Dice))
        , ("Docs", (To Docs))
        , ("About", (To About))
        ]
        (To Dice)
    , editor = Editor.init
    }


-- UPDATE
type Action = To Page
            | Edit Editor.Action

update : Action -> Model -> Model
update action model =
    case action of
        (To page)  -> { model | activePage <- page, navBar <- Navbar.update model.navBar (To page) }
        (Edit act) -> { model | editor <- Editor.update act model.editor }


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
