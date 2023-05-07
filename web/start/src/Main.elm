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

type Consistency
  = None   -- pure randomization
  | Simple -- seed initialization
  | Full   -- value cache

type Generator
  = Default String          -- regex
  | Integer Int Int         -- min, max
  | Decimal Float Float Int -- min, max, precision
  | Date    String String   -- min, max
  | Ref     String          -- uri
  | Custom  String          -- template

type alias FieldDefinition =
  { name : String
  , generator : Generator
  , consistency : Consistency
  }


type alias Model = List FieldDefinition

init : () -> ( Model, Cmd Msg )
init _ = ([
      FieldDefinition "string" (Default "[A-Z]{10}") None
    , FieldDefinition "integer" (Integer 0 100) None
    , FieldDefinition "decimal" (Decimal 0.0 1.0 2) None
    , FieldDefinition "date" (Date "1970-01-01T00:00:00Z" "2020-01-01T00:00:00Z") None
    , FieldDefinition "referential" (Ref "pimo://nameFR") None
    , FieldDefinition "custom" (Custom "{{.surname | NoAccent | upper}}.{{.name | NoAccent | lower}}@gmail.com") None
  ], Cmd.none )



-- UPDATE

type Msg
  = AddFieldDefinition

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddFieldDefinition ->
      ( model ++ [FieldDefinition "" (Default "") None], Cmd.none )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "PIMO Start"
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
  case f.generator of
  Default regex ->
    div [] [
       input [ type_ "text", value f.name ] []
      ,input [ type_ "text", value regex ] []
    ]
  Integer min max ->
    div [] [
       input [ type_ "text", value f.name ] []
      ,input [ type_ "text", value (String.fromInt min) ] []
      ,input [ type_ "text", value (String.fromInt max) ] []
    ]
  Decimal min max precision ->
    div [] [
       input [ type_ "text", value f.name ] []
      ,input [ type_ "text", value (String.fromFloat min) ] []
      ,input [ type_ "text", value (String.fromFloat max) ] []
      ,input [ type_ "text", value (String.fromInt precision) ] []
    ]
  Date min max ->
    div [] [
       input [ type_ "text", value f.name ] []
      ,input [ type_ "text", value min ] []
      ,input [ type_ "text", value max ] []
    ]
  Ref uri ->
    div [] [
       input [ type_ "text", value f.name ] []
      ,input [ type_ "text", value uri ] []
    ]
  Custom template ->
    div [] [
       input [ type_ "text", value f.name ] []
      ,input [ type_ "text", value template ] []
    ]
