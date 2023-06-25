module Model.Yaml exposing (..)

import Dict exposing (Dict)
import Yaml.Decode exposing (..)


type alias Config =
    { version : String
    , seed : Maybe Int
    , caches : Maybe (Dict String Cache)
    , maskings : List Masking
    }


type alias Cache =
    { unique : Maybe Bool
    , reverse : Maybe Bool
    }


type alias Masking =
    { selector : Selector
    , masks : List Mask
    , seed : Maybe Seed
    , cache : Maybe String
    , preserve : Maybe Preserve
    }


type alias Selector =
    { jsonpaths : List String
    }


type Mask
    = Add { value : Value }
    | Regex { pattern : String }
    | Constant { value : Value }
    | Choice { choices : List Value }
    | ChoiceRef { uri : String }


type alias Seed =
    { field : String
    }


type Preserve
    = Null
    | Empty
    | Blank



-- DECODER


cacheDecoder : Decoder Cache
cacheDecoder =
    map2 Cache
        (maybe (field "unique" bool))
        (maybe (field "reverse" bool))


maskingDecoder : Decoder Masking
maskingDecoder =
    map5 Masking
        selectorDecoder
        (at [ "masks" ] (list maskDecoder))
        (maybe (at [ "seed" ] seedDecoder))
        (maybe (at [ "cache" ] string))
        (maybe preserveDecoder)


selectorDecoder : Decoder Selector
selectorDecoder =
    map Selector
        (oneOf
            [ stringDecoderToListDecoder (at [ "selector" ] (field "jsonpath" string))
            , at [ "selectors" ] (list (field "jsonpath" string))
            ]
        )


seedDecoder : Decoder Seed
seedDecoder =
    map Seed (at [ "field" ] string)


stringDecoderToListDecoder : Decoder String -> Decoder (List String)
stringDecoderToListDecoder d =
    let
        stringToList : String -> Result String (List String)
        stringToList value =
            Ok [ value ]
    in
    d |> andThen (stringToList >> fromResult)


configDecoder : Decoder Config
configDecoder =
    map4 Config
        (field "version" string)
        (maybe (field "seed" int))
        (maybe (at [ "caches" ] (dict cacheDecoder)))
        (at [ "masking" ] (list maskingDecoder))


preserveDecoder : Decoder Preserve
preserveDecoder =
    let
        checkPreserve : String -> Result String Preserve
        checkPreserve value =
            if value == "null" then
                Ok Null

            else if value == "empty" then
                Ok Empty

            else if value == "blank" then
                Ok Blank

            else
                Err "invalid preserve value"
    in
    field "preserve" string |> andThen (checkPreserve >> fromResult)


maskDecoder : Decoder Mask
maskDecoder =
    oneOf
        [ maskAddDecoder, maskRegexDecoder ]


maskRegexDecoder : Decoder Mask
maskRegexDecoder =
    let
        create : String -> Result String { pattern : String }
        create pattern =
            Ok { pattern = pattern }
    in
    map Regex (field "regex" string |> andThen (create >> fromResult))


maskAddDecoder : Decoder Mask
maskAddDecoder =
    let
        create : Value -> Result String { value : Value }
        create value =
            Ok { value = value }
    in
    map Add
        (field "add" value |> andThen (create >> fromResult))


decodeConfig : String -> Result Error Config
decodeConfig yaml =
    fromString
        configDecoder
        yaml


decodeConfigOrNothing : String -> Maybe Config
decodeConfigOrNothing yaml =
    case decodeConfig yaml of
        Ok c ->
            Just c

        Err error ->
            let
                _ =
                    case error of
                        Parsing msg ->
                            Debug.log msg

                        Decoding msg ->
                            Debug.log msg
            in
            Nothing
