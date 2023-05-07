import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias FieldDefinition =
  { name : String,
    generator : String
  }


type alias Model = List FieldDefinition

init : Model
init = [FieldDefinition "test" "test",FieldDefinition "test2" "test2"]



-- UPDATE

type Msg
  = AddFieldDefinition

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddFieldDefinition ->
      FieldDefinition "name" "generator" :: model



-- VIEW

view : Model -> Html Msg
view model =
  div []
    (List.map viewFieldDefinition model)

viewFieldDefinition : FieldDefinition -> Html Msg
viewFieldDefinition f =
  text f.name
