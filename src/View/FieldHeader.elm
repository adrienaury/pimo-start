module View.FieldHeader exposing (..)

import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw
import Tailwind.Theme as Tw

header : String -> Styled.Html msg
header label =
    Styled.header
        [ Attr.css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_5
            ]
        ]
        [ Styled.img
            [ Attr.src "https://placehold.co/40"
            ]
            []
        , Styled.h2
            [ Attr.css
                [ Tw.capitalize
                , Tw.font_bold
                , Tw.text_lg
                , Tw.grow
                , Tw.leading_10
                ]
            ]
            [ Styled.text label ]
        , Styled.img
            [ Attr.src "https://placehold.co/20"
            , Attr.alt "remove"
            , Attr.css
                [ Tw.h_fit
                ]
            ]
            []
        ]
