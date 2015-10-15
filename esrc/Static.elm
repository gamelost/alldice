module Static (viewFooter, viewAbout, viewDocumentation) where

import Html exposing (..)
import Html.Shorthand exposing (..)
import Html.Attributes exposing (class, id, attribute)


viewFooter : Html
viewFooter =
    footer' { class = "footer" }
        [ div' { class = "container-fluid" }
            [ p' { class = "text-muted" }
                [ text "Copyright © Anja Berens"
                ]
            ]
        ]


viewAbout : Html
viewAbout =
    div [ class "panel panel-default", id "about" ]
        [ div' {class = "panel-body"}
            [ text "About stuff"
            ]
        ]


viewDocumentation : Html
viewDocumentation =
    div [ class "container-fluid", id "content" ]
        [ div' {class = "row"}
            [ div [ class "col-xs-9", attribute "role" "main" ] viewContent
            , nav [ class "col-xs-3 bs-docs-sidebar", attribute "role" "complementary" ]
                [ ul [ class "nav nav-stacked", id "sidebar" ] viewContentNav
                ]
            ]
        ]


viewContent : List Html
viewContent =
    [ section' {id = "GroupA", class = "group"}
        [ h3_ "Group A"
        , div [ class "subgroup", id "GroupASub1" ]
            [ h4_ "Group A Sub 1" ]
        , div [ class "subgroup", id "GroupASub2" ]
            [ h4_ "Group A Sub 2" ]
        ]
    , section' {id = "GroupB", class = "group"}
        [ h3_ "Group B"
        , div [ class "subgroup", id "GroupBSub1" ]
            [ h4_ "Group B Sub 1" ]
        , div [ class "subgroup", id "GroupBSub2" ]
            [ h4_ "Group B Sub 2" ]
        ]
    , section' {id = "GroupC", class = "group"}
        [ h3_ "Group C"
        , div [ class "subgroup", id "GroupCSub1" ]
            [ h4_ "Group C Sub 1" ]
        , div [ class "subgroup", id "GroupCSub2" ]
            [ h4_ "Group C Sub 2" ]
        ]
    ]


-- TODO: mostly work but top-level navs (#GroupA) seems busted
viewContentNav : List Html
viewContentNav =
    [ li_
        [ a_ "#GroupA" "Group A"
        , ul' {class = "nav nav-stacked"}
            [ li_
                [ a_ "#GroupASub1" "Sub-Group 1"
                , a_ "#GroupASub2" "Sub-Group 2"
                ]
            ]
        ]
    , li_
        [ a_ "#GroupB" "Group B"
        , ul' {class = "nav nav-stacked"}
            [ li_
                [ a_ "#GroupBSub1" "Sub-Group 1"
                , a_ "#GroupBSub2" "Sub-Group 2"
                ]
            ]
        ]
    , li_
        [ a_ "#GroupC" "Group C"
        , ul' {class = "nav nav-stacked"}
            [ li_
                [ a_ "#GroupCSub1" "Sub-Group 1"
                , a_ "#GroupCSub2" "Sub-Group 2"
                ]
            ]
        ]
    ]
