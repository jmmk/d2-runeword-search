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


minBounds : Sockets -> Int -> Bool
minBounds min sockets =
  case min of
    AnySockets ->
      True

    Sockets i ->
      sockets >= i


maxBounds : Sockets -> Int -> Bool
maxBounds max sockets =
  case max of
    AnySockets ->
      True

    Sockets i ->
      sockets <= i


socketBounds : Sockets -> Sockets -> Int -> Bool
socketBounds min max sockets =
  minBounds min sockets && maxBounds max sockets


fromString : String -> Sockets
fromString key =
  case (Dict.get key sockets) of
    Just socketType ->
      socketType

    _ ->
      AnySockets
