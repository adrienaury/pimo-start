module Model.YamlEncode exposing (..)

import Model.Yaml exposing (Config)
import Yaml.Encode exposing (..)



-- ENCODERS


configEncoder : Config -> Encoder
configEncoder config =
    record
        ([ ( "version", string config.version ) ]
            ++ seedEncoder config.seed
        )


seedEncoder : Maybe Int -> List ( String, Encoder )
seedEncoder seed =
    case seed of
        Just s ->
            [ ( "seed", int s ) ]

        Nothing ->
            []


encodeConfig : Maybe Config -> String
encodeConfig config =
    case config of
        Just c ->
            toString 2 (configEncoder c)

        Nothing ->
            "n/a"
