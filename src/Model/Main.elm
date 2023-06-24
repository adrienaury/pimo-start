module Model.Main exposing (..)

type Preserve
  = None
  | Null
  | Empty
  | Blank

type Generator
  = Regex        String                             -- pattern
--   | Integer      Int Int                            -- min, max
--   | Sequence     Int Int                            -- start, increment
--   | Boolean      Int Int                            -- weightTrue, weightFalse
--   | Decimal      Float Float Int                    -- min, max, precision
--   | DateTime     String String String               -- min, max, format
--   | ChoiceRef    String                             -- uri
--   | ChoiceRefCsv String Bool String String Int Bool -- uri, hasHeader, separator, comment, fieldNumber, doTrim
--   | Template     String                             -- template

type alias FieldDefinition =
  { name : String
  , generator : Generator
  , synthesize : Bool
  , transient : Bool
  , preserve : Preserve
  , coherent_with : List String
  , avoid_collisions : Bool
  }

type alias Model =
  { fields : List FieldDefinition
  }

newRegex : FieldDefinition
newRegex = (FieldDefinition "" (Regex "") False False None [] False)
