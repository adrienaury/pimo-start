module Model.YamlEncode exposing (..)

import Dict exposing (Dict)
import Model.Yaml exposing (Cache, Config)
import Yaml.Encode exposing (..)



-- ENCODERS


configEncoder : Config -> Encoder
configEncoder config =
    record
        ([ ( "version", string config.version ) ]
            ++ seedEncoder config.seed
            ++ cachesEncoder config.caches
        )


seedEncoder : Maybe Int -> List ( String, Encoder )
seedEncoder seed =
    case seed of
        Just s ->
            [ ( "seed", int s ) ]

        Nothing ->
            []


cachesEncoder : Maybe (Dict String Cache) -> List ( String, Encoder )
cachesEncoder caches =
    case caches of
        Just c ->
            [ ( "caches", dict identity cacheEncoder c ) ]

        Nothing ->
            []


cacheEncoder : Cache -> Encoder
cacheEncoder cache =
    record
        ([]
            ++ optionalBoolEncoder cache.unique "unique"
            ++ optionalBoolEncoder cache.reverse "reverse"
        )


optionalBoolEncoder : Maybe Bool -> String -> List ( String, Encoder )
optionalBoolEncoder value name =
    case value of
        Just s ->
            [ ( name, bool s ) ]

        Nothing ->
            []


encodeConfig : Maybe Config -> String
encodeConfig config =
    case config of
        Just c ->
            toString 2 (configEncoder c)

        Nothing ->
            "n/a"
