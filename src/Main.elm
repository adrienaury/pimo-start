module Main exposing (..)

import Browser
import Css.Global
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw
import Tailwind.Theme as Tw

import View.Main as View
import Model.Main exposing (..)
import Msg exposing (..)
import Yaml

-- MAIN

main : Program () Model Msg
main =
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- INIT

init : () -> ( Model, Cmd Msg )
init _ = (
  { fields = [ ]
  }, updateYaml (Yaml.file []) )

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "PIMO Start"
  , body = [
      Html.toUnstyled <|
          Html.div [ Attr.css [ Tw.bg_color Tw.gray_50 ] ]
              [ -- This will give us the standard tailwind style-reset as well as the fonts
                Css.Global.global Tw.globalStyles
              , View.mainView model
              ]
  ]
  }