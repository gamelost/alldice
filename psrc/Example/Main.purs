module Example.Main where

import Debug.Trace
import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

-- To implement additional attribute types
import qualified Thermite.Internal as I


foreign import data ScrollSpy :: !

foreign import scrollspy
    "function scrollspy() { \
    \   $('body').scrollspy({ \
    \       target: '.bs-docs-sidebar', \
    \       offset: 40 \
    \   }); \
    \}" :: forall r. Eff (scrollspy :: ScrollSpy | r) Unit

foreign import data Affix :: !

foreign import affix
    "function affix() { \
    \   $('#sidebar').affix({ \
    \       offset: { \
    \           top: 60 \
    \       } \
    \   }); \
    \}" :: forall r. Eff (affix :: Affix | r) Unit


-- Footer renderer
footer :: T.Html _
footer =
    T.footer (A.className "footer")
        [ T.div (A.className "container-fluid")
            [ T.p (A.className "text-muted")
                [ T.text "Copyright © Anja Berens"
                ]
            ]
        ]

-- Navbar renderer
dataToggle :: String -> T.Attr
dataToggle = I.unsafeAttribute "data-toggle"

dataTarget :: String -> T.Attr
dataTarget = I.unsafeAttribute "data-target"

ariaExpanded :: String -> T.Attr
ariaExpanded = I.unsafeAttribute "aria-expanded"

ariaControls :: String -> T.Attr
ariaControls = I.unsafeAttribute "aria-controls"

-- TODO: Property - which navbar element is active
navbar :: T.Html _
navbar =
    T.nav (A.className "navbar navbar-default navbar-static-top")
        [ T.div (A.className "container-fluid")
            [ T.div (A.className "navbar-header")
                -- TODO: tweak when it collapses cos it collapses too soon at the moment
                -- Customize the @grid-float-breakpoint variable or add your own media query.
                [ T.button (A._type "button" <>
                            A.className "navbar-toggle collapsed" <>
                            dataToggle "collapse" <>
                            dataTarget "#navbar" <>
                            ariaExpanded "false" <>
                            ariaControls "navbar")
                    [ T.span (A.className "sr-only") [ T.text "Toggle navigation" ]
                    , T.span (A.className "icon-bar") []
                    , T.span (A.className "icon-bar") []
                    , T.span (A.className "icon-bar") []
                ]
            , T.a (A.className "navbar-brand" <> A.href "index.html") [ T.text "AllDice" ]
            ]
        , T.div (A._id "navbar" <> A.className "navbar-collapse collapse")
            [ T.ul (A.className "nav navbar-nav navbar-right")
                [ T.li (A.className "active") [ T.a (A.href "index.html") [ T.text "Dice" ] ]
                , T.li' [ T.a (A.href "documentation.html") [ T.text "Docs" ] ]
                , T.li' [ T.a (A.href "about.html") [ T.text "About" ] ]
                ]
            ]
        ]
    ]

-- Editor Renderer
formFor :: String -> T.Attr
formFor = I.unsafeAttribute "for"

-- TODO: state - editor stuff i suspect
-- TODO: default content of textarea (currently undefined, we probably want empty here)
-- TODO: property for available dice type, available data view
editor :: T.Html _
editor =
    T.div (A._id "diceInput")
        [ T.form'
            [ T.div (A.className "form-group")
                [ T.label (formFor "diceProgram") [ T.text "Dice Program" ]
                , T.textarea (A._id "diceProgram" <>
                              A.className "form-control" <>
                              A._type "text" <>
                              A.rows "10") []
                ]
            , T.div (A.className "form-inline")
                [ T.button (A._type "submit" <> A.className "btn btn-default") [ T.text "Submit" ]
                , T.div (A.className "form-group")
                    [ T.label (formFor "diceType") [ T.text "Program Type" ]
                    , T.select (A._id "diceType" <> A.className "form-control")
                        [ T.option' [ T.text "Scheme" ]
                        , T.option' [ T.text "PCGen" ]
                        ]
                    ]
                , T.div (A.className "form-group")
                    [ T.label (formFor "dataView") [ T.text "View" ]
                    , T.select (A._id "dataView" <> A.className "form-control")
                        [ T.option' [ T.text "Table" ]
                        , T.option' [ T.text "Graph" ]
                        ]
                    ]
                ]
            ]
        ]


-- Result Renderer
-- TODO: Property - crapton (data/etc)
result :: T.Html _
result =
    T.div (A._id "diceResult" <> A.className "panel panel-default")
        [ T.div (A.className "panel-body")
            [ T.text "Foobar"
            ]
        ]



-- React bits
data Action = DoNothing

data State = State { editText :: String }

initialState :: State
initialState = State { editText: "" }

performAction :: T.PerformAction _ State _ Action
performAction _ action = T.modifyState (updateState action)
  where
    updateState :: Action -> State -> State
    updateState DoNothing = id

render :: T.Render _ State _ Action
render _ _ _ _ = T.body' [navbar, T.div (A.className "container-fluid") [editor, result], footer]

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
-- React bits


main = do
    trace "Setup reactjs"
    let component = T.createClass spec
    T.render component {}

    trace "Setup bootstrap"
    scrollspy
    affix
