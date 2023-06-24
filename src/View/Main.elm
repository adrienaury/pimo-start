module View.Main exposing (..)

import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw
import Tailwind.Theme as Tw
import List

import Msg exposing (..)
import View.Field as View
import View.Footer as View
import View.Masking as View
import Model.Main exposing (..)

mainView : Model -> Styled.Html Msg
mainView model =
    Styled.main_
        [ Attr.css
            [ Tw.flex
            , Tw.flex_row
            ]
        ]
        [ Styled.section
            [ Attr.css
                [ Tw.grow, Tw.w_1over2
                ]
            ]
            (List.indexedMap View.field model.fields             ++ [View.footer])
        , Styled.section
            [ Attr.css
                [ Tw.grow
                ]
            ]
            [ View.masking
            ]
        ]
