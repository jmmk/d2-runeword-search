module Patch exposing (..)


type Patch
  = OneNine
  | OneTen
  | OneEleven


toString : Patch -> String
toString patch =
  case patch of
    OneNine ->
      "1.09"

    OneTen ->
      "1.10"

    OneEleven ->
      "1.11"


fromString : String -> Patch
fromString string =
  case string of
    "1.10" ->
      OneTen

    "1.11" ->
      OneEleven

    _ ->
      OneNine
