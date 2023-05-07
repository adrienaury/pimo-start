module Main exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN

main : Program () Model Msg
main =
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- MODEL

type alias FieldDefinition =
  { name : String,
    generator : String
  }


type alias Model = List FieldDefinition

init : () -> ( Model, Cmd Msg )
init _ = ([FieldDefinition "test" "test",FieldDefinition "test2" "test2"], Cmd.none )



-- UPDATE

type Msg
  = AddFieldDefinition

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddFieldDefinition ->
      ( model ++ [FieldDefinition "name" "generator"], Cmd.none )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body = [
        div [] [
            div []
            (List.map viewFieldDefinition model),
            button [ onClick AddFieldDefinition ] [ text "+" ]
        ]
    ]
  }

viewFieldDefinition : FieldDefinition -> Html Msg
viewFieldDefinition f =
  div [] [
    input [ type_ "text", value f.name ] [],
    input [ type_ "text", value f.generator ] []
  ]
