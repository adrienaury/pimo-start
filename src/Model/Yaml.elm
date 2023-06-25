module Model.Yaml exposing (..)

import Dict exposing (Dict)
import Yaml.Decode exposing (..)


type alias Config =
    { version : String
    , seed : Maybe Int
    , caches : Dict String Cache

    -- , maskings : List Masking
    }


type alias Cache =
    { unique : Maybe Bool
    , reverse : Maybe Bool
    }


type alias Masking =
    { selector : Selector
    , masks : List Mask
    , seed : Seed
    }


type alias Selector =
    { jsonpaths : List String
    }


type Mask
    = Regex { pattern : String }
    | Constant { value : String }
    | Choice { choices : List Value }
    | ChoiceRef { uri : String }


type alias Seed =
    { field : String
    }


type Value
    = Int
    | String



-- DECODER


cacheDecoder : Decoder Cache
cacheDecoder =
    map2 Cache
        (maybe (field "unique" bool))
        (maybe (field "reverse" bool))


maskingDecoder : Decoder Masking
maskingDecoder =
    map3 Masking
        (succeed (Selector []))
        (succeed [])
        (succeed { field = "" })


configDecoder : Decoder Config
configDecoder =
    map3 Config
        (field "version" string)
        (maybe (field "seed" int))
        (at [ "caches" ] (dict cacheDecoder))


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

        Err _ ->
            Nothing
