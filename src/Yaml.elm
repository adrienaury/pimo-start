module Yaml exposing (..)

import Model.Main exposing (..)

file : List FieldDefinition -> String
file fields =
  "version: \"1\"" ++ caches fields ++ """
masking:
""" ++ String.concat (List.map masking fields)


masking : FieldDefinition -> String
masking field =
  selector field ++ mask field

selector : FieldDefinition -> String
selector field =
  String.replace "<name>" field.name """
  - selector:
      jsonpath: "<name>"
"""

mask : FieldDefinition -> String
mask field =
  """    masks:
""" ++ synthesize field ++ transient field ++ maskRegex field ++ preserve field

synthesize : FieldDefinition -> String
synthesize field =
  if field.synthesize then """      - add: true
""" else ""

transient : FieldDefinition -> String
transient field =
  if field.transient then """      - add-transient: true
""" else ""

preserve : FieldDefinition -> String
preserve field =
  case field.preserve of
    None -> ""
    Null -> "    preserve: \"null\"\n"
    Empty -> "    preserve: \"empty\"\n"
    Blank -> "    preserve: \"blank\"\n"

maskRegex : FieldDefinition -> String
maskRegex field =
  coherence_seed field (
  case field.generator of
    Regex pattern -> String.replace "<pattern>" pattern """      - regex: "<pattern>"
"""
  )

coherence_seed : FieldDefinition -> String -> String
coherence_seed field generator =
  if List.isEmpty field.coherent_with then generator else (
    if field.avoid_collisions
      then coherence_cache field generator
      else coherence_source field.coherent_with ++ generator ++ "    seed:\n      field: \"" ++ field.name ++ "\"\n"
  )

coherence_cache : FieldDefinition -> String -> String
coherence_cache field generator =
  if List.isEmpty field.coherent_with then generator else (
    coherence_source field.coherent_with ++ generator ++ "    cache: \"" ++ field.name ++ "\"\n"
  )

coherence_source : List String -> String
coherence_source fieldnames =
  "      - template: \"" ++
  String.concat (List.map fieldname_in_template fieldnames) ++ "\"\n"

fieldname_in_template : String -> String
fieldname_in_template name =
  "{{." ++ name ++ "}}"

caches : List FieldDefinition -> String
caches fields =
  let
    needCache field =
      field.avoid_collisions && List.length field.coherent_with > 0
    fieldsWithCache = List.filter needCache fields
  in
    if List.length fieldsWithCache > 0
      then "\ncaches:\n" ++ String.concat (List.map cache fieldsWithCache)
      else ""

cache : FieldDefinition -> String
cache field =
  "  " ++ field.name ++ ":\n    unique: true\n"
