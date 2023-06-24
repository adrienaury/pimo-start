module View.FieldDetails exposing (..)

import Css
import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Evt
import Tailwind.Utilities as Tw
import Tailwind.Theme as Tw

import Msg exposing (..)

details : Int -> Styled.Html Msg
details i =
    Styled.details
        [ 
        ]
        [ Styled.summary []
            [ Styled.text "Advanced options" ]
        , Styled.div
            [ Attr.css
                [ Tw.grid
                , Css.property "grid-template-columns" "25% 75%"
                , Tw.gap_1
                ]
            ]
            [ Styled.label
                [ Attr.for ("synthesize" ++ (String.fromInt i))
                , Attr.css
                    [ Tw.font_medium
                    ]
                ]
                [ Styled.text "Synthesize" ]
            , Styled.select
                [ Attr.id ("synthesize" ++ (String.fromInt i))
                , Evt.onInput (ChangeSynthesize i)
                , Attr.css
                    [ Tw.px_2
                    , Css.focus
                        [ Tw.bg_color Tw.stone_200
                        ]
                    , Css.hover
                        [ Tw.bg_color Tw.stone_50
                        ]
                    ]
                ]
                [ Styled.option
                    [ Attr.value "false"
                    ]
                    [ Styled.text "no" ]
                , Styled.option
                    [ Attr.value "true"
                    ]
                    [ Styled.text "yes" ]
                ]
            , Styled.label
                [ Attr.for ("transient" ++ (String.fromInt i))
                , Attr.css
                    [ Tw.font_medium
                    ]
                ]
                [ Styled.text "Transient" ]
            , Styled.select
                [ Attr.id ("transient" ++ (String.fromInt i))
                , Evt.onInput (ChangeTransient i)
                , Attr.css
                    [ Tw.px_2
                    , Css.focus
                        [ Tw.bg_color Tw.stone_200
                        ]
                    , Css.hover
                        [ Tw.bg_color Tw.stone_50
                        ]
                    ]
                ]
                [ Styled.option
                    [ Attr.value "false"
                    ]
                    [ Styled.text "no" ]
                , Styled.option
                    [ Attr.value "true"
                    ]
                    [ Styled.text "yes" ]
                ]
            , Styled.label
                [ Attr.for ("preserve" ++ (String.fromInt i))
                , Attr.css
                    [ Tw.font_medium
                    ]
                ]
                [ Styled.text "Preserve" ]
            , Styled.select
                [ Attr.id ("preserve" ++ (String.fromInt i))
                , Evt.onInput (ChangePreserve i)
                , Attr.css
                    [ Tw.px_2
                    , Css.focus
                        [ Tw.bg_color Tw.stone_200
                        ]
                    , Css.hover
                        [ Tw.bg_color Tw.stone_50
                        ]
                    ]
                ]
                [ Styled.option
                    [ Attr.value "none"
                    ]
                    [ Styled.text "none" ]
                , Styled.option
                    [ Attr.value "null"
                    ]
                    [ Styled.text "null" ]
                , Styled.option
                    [ Attr.value "empty"
                    ]
                    [ Styled.text "empty" ]
                , Styled.option
                    [ Attr.value "blank"
                    ]
                    [ Styled.text "blank" ]
                ]
            , Styled.label
                [ Attr.for ("coherent_with" ++ (String.fromInt i))
                , Attr.css
                    [ Tw.font_medium
                    ]
                ]
                [ Styled.text "Coherent with" ]
            , Styled.input
                [ Attr.id ("coherent_with" ++ (String.fromInt i))
                , Attr.type_ "text"
                , Attr.placeholder "field1, field2"
                , Evt.onInput (ChangeCoherentWith i)
                , Attr.css
                    [ Tw.px_3
                    , Css.focus
                        [ Tw.bg_color Tw.stone_200
                        ]
                    , Css.hover
                        [ Tw.bg_color Tw.stone_50
                        ]
                    ]
                ]
                []
            , Styled.label
                [ Attr.for ("collisions" ++ (String.fromInt i))
                , Attr.css
                    [ Tw.font_medium
                    ]
                ]
                [ Styled.text "Collisions" ]
            , Styled.select
                [ Attr.id ("collisions" ++ (String.fromInt i))
                , Evt.onInput (ChangeAvoidCollisions i)
                , Attr.css
                    [ Tw.px_2
                    , Css.focus
                        [ Tw.bg_color Tw.stone_200
                        ]
                    , Css.hover
                        [ Tw.bg_color Tw.stone_50
                        ]
                    ]
                ]
                [ Styled.option
                    [ Attr.value "allowed"
                    ]
                    [ Styled.text "allowed" ]
                , Styled.option
                    [ Attr.value "forbidden"
                    ]
                    [ Styled.text "forbidden" ]
                ]
            ]
        ]