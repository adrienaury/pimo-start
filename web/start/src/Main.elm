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
      FieldDefinition "first_name" (Ref "pimo://nameFR") None
    , FieldDefinition "last_name" (Ref "pimo://surnameFR") None
    , FieldDefinition "email" (Custom "{{ .first_name | lower | NoAccent }}.{{ .last_name | lower | NoAccent }}@yopmail.fr") None
    , FieldDefinition "birthdate" (Date "1970-01-01T00:00:00Z" "2020-01-01T00:00:00Z") None
  ], Cmd.none )



-- UPDATE

type Msg
  = AddDefault
  | AddInteger
  | AddDecimal
  | AddDate
  | AddRef
  | AddCustom

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddDefault ->
      ( model ++ [FieldDefinition "" (Default "") None], Cmd.none )
    AddInteger ->
      ( model ++ [FieldDefinition "" (Integer 0 100) None], Cmd.none )
    AddDecimal ->
      ( model ++ [FieldDefinition "" (Decimal 0.0 1.0 2) None], Cmd.none )
    AddDate ->
      ( model ++ [FieldDefinition "" (Date "" "") None], Cmd.none )
    AddRef ->
      ( model ++ [FieldDefinition "" (Ref "") None], Cmd.none )
    AddCustom ->
      ( model ++ [FieldDefinition "" (Custom "") None], Cmd.none )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "PIMO Start"
  , body = [
        div [] [
            div []
            (List.map viewFieldDefinition model)
            , button [ onClick AddDefault ] [ text "+ Regex" ]
            , button [ onClick AddInteger ] [ text "+ Int" ]
            , button [ onClick AddDecimal ] [ text "+ Decimal" ]
            , button [ onClick AddDate ] [ text "+ Date" ]
            , button [ onClick AddRef ] [ text "+ Ref" ]
            , button [ onClick AddCustom ] [ text "+ Template" ]
        ]
    ]
  }

viewFieldDefinition : FieldDefinition -> Html Msg
viewFieldDefinition f =
  case f.generator of
  Default regex ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name ] []
      ,input [ type_ "text", placeholder "Regex", value regex ] []
    ]
  Integer min max ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name ] []
      ,input [ type_ "text", placeholder "Min value", value (String.fromInt min) ] []
      ,input [ type_ "text", placeholder "Max value", value (String.fromInt max) ] []
    ]
  Decimal min max precision ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name ] []
      ,input [ type_ "text", placeholder "Min value", value (String.fromFloat min) ] []
      ,input [ type_ "text", placeholder "Max value", value (String.fromFloat max) ] []
      ,input [ type_ "text", placeholder "Precision", value (String.fromInt precision) ] []
    ]
  Date min max ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name ] []
      ,input [ type_ "text", placeholder "Min date", value min ] []
      ,input [ type_ "text", placeholder "Max date", value max ] []
    ]
  Ref uri ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name ] []
      ,input [ type_ "text", placeholder "Referential URI", value uri ] []
    ]
  Custom template ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name ] []
      ,input [ type_ "text", placeholder "Template", value template ] []
    ]
