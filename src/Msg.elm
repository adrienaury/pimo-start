port module Msg exposing (..)

import Model.Main exposing (..)
import Model.Utilities exposing (..)
import Yaml

-- PORTS

port updateYaml : String -> Cmd msg

-- MESSAGES

type Msg
    = Add FieldDefinition              -- add a new field to the list
    | ChangeName Int String            -- update field name (index in the list, new name)
    | ChangeSynthesize Int String      -- update field synthesize option
    | ChangeTransient Int String       -- update field transient option
    | ChangePreserve Int String        -- update field preserve option
    | ChangeCoherentWith Int String    -- update field coherent_with option
    | ChangeAvoidCollisions Int String -- update field avoid_collisions option
    | ChangeRegexPattern Int String    -- update regex pattern

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    Add field ->
      let
        newFields = model.fields ++ [field]
        newYaml = Yaml.file newFields
      in
        ({model | fields = newFields }, updateYaml newYaml)

    ChangeName i name ->
      let
        newModel = updateFieldName model i name
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )

    ChangeSynthesize i value ->
      let
        newModel = updateFieldSynthesize model i (if value == "true" then True else False) 
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )

    ChangeTransient i value ->
      let
        newModel = updateFieldTransient model i (if value == "true" then True else False)
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )

    ChangePreserve i value ->
      let
        newModel = updateFieldPreserve model i (if value == "null" then Null else if value == "empty" then Empty else if value == "blank" then Blank else None)
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )

    ChangeCoherentWith i value ->
      let
        newModel = updateFieldCoherentWith model i (List.filter isNotEmpty (List.map String.trim (String.split "," value)))
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )

    ChangeAvoidCollisions i value ->
      let
        newModel = updateFieldAvoidCollisions model i (if value == "forbidden" then True else False)
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )

    ChangeRegexPattern i pattern ->
      let
        newModel = updateFieldRegexPattern model i pattern
        newYaml = Yaml.file newModel.fields
      in
        ( newModel, updateYaml newYaml )
