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
    | AddTransient { value : Value }
    | Template { value : String }
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
