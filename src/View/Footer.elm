module View.Footer exposing (..)

import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw
import Tailwind.Theme as Tw

import Msg exposing (..)
import View.Button as View
import Model.Main exposing (newRegex)

footer : Styled.Html Msg
footer =
    Styled.footer
        [ Attr.css
            [ Tw.grid
            , Tw.grid_cols_4
            , Tw.gap_1
            , Tw.leading_4
            , Tw.font_light
            ]
        ]
        [
            View.button "+regex" newRegex,
            View.button "+integer" newRegex,
            View.button "+decimal" newRegex,
            View.button "+bool" newRegex,
            View.button "+datetime" newRegex,
            View.button "+choice" newRegex,
            View.button "+choice_ref" newRegex,
            View.button "+sequence" newRegex
        ]
