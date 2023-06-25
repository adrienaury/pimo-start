module Model.YamlDecode exposing (..)

import Model.Yaml exposing (..)
import Yaml.Decode exposing (..)


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
        [ maskAddDecoder, maskAddTransientDecoder, maskConstantDecoder, maskRegexDecoder ]



-- MASKS


maskAddDecoder : Decoder Mask
maskAddDecoder =
    let
        create : Value -> Result String { value : Value }
        create value =
            Ok { value = value }
    in
    map Add
        (field "add" value |> andThen (create >> fromResult))


maskAddTransientDecoder : Decoder Mask
maskAddTransientDecoder =
    let
        create : Value -> Result String { value : Value }
        create value =
            Ok { value = value }
    in
    map AddTransient
        (field "add-transient" value |> andThen (create >> fromResult))


maskConstantDecoder : Decoder Mask
maskConstantDecoder =
    let
        create : Value -> Result String { value : Value }
        create value =
            Ok { value = value }
    in
    map Constant
        (field "constant" value |> andThen (create >> fromResult))


maskRegexDecoder : Decoder Mask
maskRegexDecoder =
    let
        create : String -> Result String { pattern : String }
        create pattern =
            Ok { pattern = pattern }
    in
    map Regex (field "regex" string |> andThen (create >> fromResult))


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
