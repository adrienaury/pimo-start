module View.Masking exposing (..)

import Css
import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw
import Tailwind.Theme as Tw

masking : Styled.Html msg
masking =
    Styled.div
            [ Attr.css [ Tw.px_5, Tw.my_2, Tw.shadow_lg, Css.property "height" "800px" ]
            , Attr.id "editor-yaml"
            ]
            []