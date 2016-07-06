module SearchType exposing (..)

import Dict exposing (Dict)


type SearchType
  = All
  | Name
  | Runes
  | Properties


searchTypes : Dict String SearchType
searchTypes =
  Dict.fromList
    [ ( "All", All )
    , ( "Name", Name )
    , ( "Runes", Runes )
    , ( "Properties", Properties )
    ]


displayList : List String
displayList =
  List.sort (Dict.keys searchTypes)


fromString : String -> SearchType
fromString key =
  case (Dict.get key searchTypes) of
    Just searchType ->
      searchType

    _ ->
      All
