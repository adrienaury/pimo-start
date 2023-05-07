port module Main exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Json.Encode as Encode



-- MAIN

main : Program () Model Msg
main =
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- PORTS

port sendMessage : Encode.Value -> Cmd msg

encodeFieldsDefinitions : Array FieldDefinition -> Encode.Value
encodeFieldsDefinitions fields =
  Encode.array encodeFieldDefinition fields

encodeFieldDefinition : FieldDefinition -> Encode.Value
encodeFieldDefinition field =
  case field.generator of
  Default regex ->
    Encode.object [
      ( "name", Encode.string field.name )
    , ( "type", Encode.string "regex" )
    , ( "regex", Encode.string regex )
    ]
  Integer min max ->
    Encode.object [
      ( "name", Encode.string field.name )
    , ( "type", Encode.string "randomInt" )
    , ( "min", Encode.int min )
    , ( "max", Encode.int max )
    ]
  Decimal min max precision ->
    Encode.object [
      ( "name", Encode.string field.name )
    , ( "type", Encode.string "randomDecimal" )
    , ( "min", Encode.float min )
    , ( "max", Encode.float max )
    , ( "precision", Encode.int precision )
    ]
  Date min max ->
    Encode.object [
      ( "name", Encode.string field.name )
    , ( "type", Encode.string "randDate" )
    , ( "min", Encode.string min )
    , ( "max", Encode.string max )
    ]
  Ref uri ->
    Encode.object [
      ( "name", Encode.string field.name )
    , ( "type", Encode.string "randomChoiceInUri" )
    , ( "uri", Encode.string uri )
    ]
  Custom template ->
    Encode.object [
      ( "name", Encode.string field.name )
    , ( "type", Encode.string "template" )
    , ( "template", Encode.string template )
    ]

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


type alias Model = Array FieldDefinition

init : () -> ( Model, Cmd Msg )
init _ = (Array.fromList [
      FieldDefinition "first_name" (Ref "pimo://nameFR") None
    , FieldDefinition "last_name" (Ref "pimo://surnameFR") None
    , FieldDefinition "email" (Custom "{{ .first_name | lower | NoAccent }}.{{ .last_name | lower | NoAccent }}@yopmail.fr") None
    , FieldDefinition "birthdate" (Date "1970-01-01T00:00:00" "2020-01-01T00:00:00") None
  ], Cmd.none )



-- UPDATE

type Msg
  = AddDefault
  | AddInteger
  | AddDecimal
  | AddDate
  | AddRef
  | AddCustom
  | ChangeFieldName Int String
  | ChangeGeneratorDefault Int String
  | ChangeGeneratorIntegerMin Int String
  | ChangeGeneratorIntegerMax Int String
  | ChangeGeneratorDecimalMin Int String
  | ChangeGeneratorDecimalMax Int String
  | ChangeGeneratorDecimalPrecision Int String
  | ChangeGeneratorDateMin Int String
  | ChangeGeneratorDateMax Int String
  | ChangeGeneratorRefUri Int String
  | ChangeGeneratorCustomTemplate Int String
  | Save

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddDefault ->
      ( Array.push (FieldDefinition "" (Default "") None) model, Cmd.none )
    AddInteger ->
      ( Array.push (FieldDefinition "" (Integer 0 100) None) model, Cmd.none )
    AddDecimal ->
      ( Array.push (FieldDefinition "" (Decimal 0.0 1.0 2) None) model, Cmd.none )
    AddDate ->
      ( Array.push (FieldDefinition "" (Date "" "") None) model, Cmd.none )
    AddRef ->
      ( Array.push (FieldDefinition "" (Ref "") None) model, Cmd.none )
    AddCustom ->
      ( Array.push (FieldDefinition "" (Custom "") None) model, Cmd.none )
    ChangeFieldName i name ->
      let
        updateFieldName index item =
          if (index == i) then
            { item | name = name }
          else
            item

        items =
          Array.indexedMap updateFieldName model
      in
        ( items, Cmd.none )
    ChangeGeneratorDefault i regex ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Default r ->
              { item | generator = Default regex}
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorIntegerMin i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Integer min max ->
              { item | generator = Integer (Maybe.withDefault 0 (String.toInt value)) max }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorIntegerMax i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Integer min max ->
              { item | generator = Integer min (Maybe.withDefault 100 (String.toInt value)) }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorDecimalMin i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Decimal min max precision ->
              { item | generator = Decimal (Maybe.withDefault 0.0 (String.toFloat value)) max precision }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorDecimalMax i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Decimal min max precision ->
              { item | generator = Decimal min (Maybe.withDefault 1.0 (String.toFloat value)) precision }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorDecimalPrecision i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Decimal min max precision ->
              { item | generator = Decimal min max (Maybe.withDefault 2 (String.toInt value)) }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorDateMin i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Date min max ->
              { item | generator = Date value max }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorDateMax i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Date min max ->
              { item | generator = Date min value }
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorRefUri i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Ref _ ->
              { item | generator = Ref value}
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    ChangeGeneratorCustomTemplate i value ->
      let
        updateGenerator index item =
          if (index == i) then
            case item.generator of
            Custom _ ->
              { item | generator = Custom value}
            _ ->
              item
          else
            item

        items =
          Array.indexedMap updateGenerator model
      in
        ( items, Cmd.none )
    Save ->
      ( model, sendMessage (encodeFieldsDefinitions model) )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "PIMO Start"
  , body = [
        div [] [
            div []
            (List.indexedMap viewFieldDefinition (Array.toList model))
            , button [ onClick AddDefault ] [ text "+ Regex" ]
            , button [ onClick AddInteger ] [ text "+ Int" ]
            , button [ onClick AddDecimal ] [ text "+ Decimal" ]
            , button [ onClick AddDate ] [ text "+ Date" ]
            , button [ onClick AddRef ] [ text "+ Ref" ]
            , button [ onClick AddCustom ] [ text "+ Template" ]
            , button [ onClick Save ] [ text "Save" ]
        ]
    ]
  }

viewFieldDefinition : Int -> FieldDefinition -> Html Msg
viewFieldDefinition i f =
  case f.generator of
  Default regex ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name, onInput (ChangeFieldName i) ] []
      ,input [ type_ "text", placeholder "Regex", value regex, onInput (ChangeGeneratorDefault i) ] []
    ]
  Integer min max ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name, onInput (ChangeFieldName i) ] []
      ,input [ type_ "text", placeholder "Min value", value (String.fromInt min), onInput (ChangeGeneratorIntegerMin i) ] []
      ,input [ type_ "text", placeholder "Max value", value (String.fromInt max), onInput (ChangeGeneratorIntegerMax i) ] []
    ]
  Decimal min max precision ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name, onInput (ChangeFieldName i) ] []
      ,input [ type_ "text", placeholder "Min value", value (String.fromFloat min), onInput (ChangeGeneratorDecimalMin i) ] []
      ,input [ type_ "text", placeholder "Max value", value (String.fromFloat max), onInput (ChangeGeneratorDecimalMax i) ] []
      ,input [ type_ "text", placeholder "Precision", value (String.fromInt precision), onInput (ChangeGeneratorDecimalPrecision i) ] []
    ]
  Date min max ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name, onInput (ChangeFieldName i) ] []
      ,input [ type_ "datetime-local", placeholder "Min date", value min, onInput (ChangeGeneratorDateMin i) ] []
      ,input [ type_ "datetime-local", placeholder "Max date", value max, onInput (ChangeGeneratorDateMax i) ] []
    ]
  Ref uri ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name, onInput (ChangeFieldName i) ] []
      ,input [ type_ "text", placeholder "Referential URI", value uri, onInput (ChangeGeneratorRefUri i) ] []
    ]
  Custom template ->
    div [] [
       input [ type_ "text", placeholder "Field Name", value f.name, onInput (ChangeFieldName i) ] []
      ,input [ type_ "text", placeholder "Template", value template, onInput (ChangeGeneratorCustomTemplate i) ] []
    ]
