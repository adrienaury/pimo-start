module Model.Utilities exposing (..)

import Model.Main exposing (..)

updateFieldName : Model -> Int -> String -> Model
updateFieldName model index name =
    let
        updater i field =
            if (index == i) then
                { field | name = name }
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

updateFieldSynthesize : Model -> Int -> Bool -> Model
updateFieldSynthesize model index value =
    let
        updater i field =
            if (index == i) then
                { field | synthesize = value }
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

updateFieldTransient : Model -> Int -> Bool -> Model
updateFieldTransient model index value =
    let
        updater i field =
            if (index == i) then
                { field | transient = value }
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

updateFieldPreserve : Model -> Int -> Preserve -> Model
updateFieldPreserve model index value =
    let
        updater i field =
            if (index == i) then
                { field | preserve = value }
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

updateFieldCoherentWith : Model -> Int -> List String -> Model
updateFieldCoherentWith model index value =
    let
        updater i field =
            if (index == i) then
                { field | coherent_with = value }
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

updateFieldAvoidCollisions : Model -> Int -> Bool -> Model
updateFieldAvoidCollisions model index value =
    let
        updater i field =
            if (index == i) then
                { field | avoid_collisions = value }
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

updateFieldRegexPattern : Model -> Int -> String -> Model
updateFieldRegexPattern model index pattern =
    let
        updater i field =
            if (index == i) then
                case field.generator of
                Regex _ ->
                    { field | generator = Regex pattern }
                --_ -> field
            else
                field
        fields = List.indexedMap updater model.fields
    in
        {model | fields = fields}

isNotEmpty : String -> Bool
isNotEmpty s =
    if String.isEmpty s then False else True
