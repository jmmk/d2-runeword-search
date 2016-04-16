module Sockets (..) where

import Dict exposing (Dict)


type Sockets
  = AnySockets
  | Sockets Int


sockets : Dict String Sockets
sockets =
  Dict.fromList
    [ ( "Any", AnySockets )
    , ( "2", Sockets 2 )
    , ( "3", Sockets 3 )
    , ( "4", Sockets 4 )
    , ( "5", Sockets 5 )
    , ( "6", Sockets 6 )
    ]


compareSockets : String -> String -> Order
compareSockets a b =
  if a == "Any" then
    LT
  else if b == "Any" then
    GT
  else
    compare a b


displayList : List String
displayList =
  List.sortWith compareSockets (Dict.keys sockets)


matchSingle : Sockets -> Int -> Bool
matchSingle selected numSockets =
  case selected of
    AnySockets ->
      True

    Sockets i ->
      i == numSockets


match : List Sockets -> Int -> Bool
match selectedSockets numSockets =
  List.any (\selected -> matchSingle selected numSockets) selectedSockets


fromString : String -> Sockets
fromString key =
  case (Dict.get key sockets) of
    Just socketType ->
      socketType

    _ ->
      AnySockets
