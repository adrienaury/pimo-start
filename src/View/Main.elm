module View.Main exposing (..)

import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import List
import Model.Main exposing (..)
import Model.YamlEncode exposing (encodeConfig)
import Msg exposing (..)
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import View.Field as View
import View.Footer as View
import View.Masking as View


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
                [ Tw.grow
                , Tw.w_1over2
                ]
            ]
            (List.indexedMap View.field model.fields ++ [ View.footer ])
        , Styled.section
            [ Attr.css
                [ Tw.grow
                ]
            ]
            [ View.masking
            ]
        , Styled.pre
            [ Attr.css
                [ Tw.grow
                ]
            ]
            [ Styled.text (encodeConfig model.test)
            ]
        ]
