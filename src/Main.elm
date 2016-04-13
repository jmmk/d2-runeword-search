module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (id, class, style, type')
import Html.Events as Events
import Effects exposing (Effects, Never)
import Task
import String
import Runewords exposing (Runeword, Patch(..), runewords)
import Dict exposing (Dict)


type alias Model =
  { keywords : Maybe (List String)
  , runewords : List Runeword
  , searchType : SearchType
  , minSockets : SocketType
  , maxSockets : SocketType
  }


initialModel : Model
initialModel =
  { keywords = Nothing
  , runewords = runewords
  , searchType = All
  , minSockets = AnySockets
  , maxSockets = AnySockets
  }


init : ( Model, Effects Action )
init =
  ( initialModel, Effects.none )


type Action
  = KeywordSearch String
  | ChangeSearchType SearchType
  | ChangeMinSockets SocketType
  | ChangeMaxSockets SocketType


renderProperties : List String -> Html
renderProperties properties =
  div
    [ class "content" ]
    [ ul
        [ style [ ( "margin-top", "0" ) ] ]
        (List.map (\p -> li [] [ text p ]) properties)
    ]


showPatch : Patch -> String
showPatch patch =
  case patch of
    OneEleven ->
      "1.11"

    OneTen ->
      "1.10"

    OneNine ->
      "1.09"


renderAttributes : Runeword -> Html
renderAttributes runeword =
  div
    [ class "content" ]
    [ h6 [] [ text ("Sockets: " ++ (toString runeword.sockets)) ]
    , h6 [] [ text ("Ladder Only: " ++ (toString runeword.ladderOnly)) ]
    , h6 [] [ text ("Patch: " ++ (showPatch runeword.patch)) ]
    , h6 [] [ text ("Required Clvl: " ++ (toString runeword.clvl)) ]
    ]


renderRuneword : Runeword -> Html
renderRuneword runeword =
  div
    [ class "box" ]
    [ div
        [ class "content is-text-centered" ]
        [ h3
            [ class "title is-3" ]
            [ text runeword.name ]
        , p
            [ class "subtitle is-5" ]
            [ text (String.join " + " (List.map toString runeword.runes)) ]
        ]
    , div
        [ class "columns" ]
        [ div
            [ class "column is-4" ]
            [ renderAttributes runeword ]
        , div
            [ class "column is-8" ]
            [ renderProperties runeword.properties ]
        ]
    ]


keywordMatch : String -> String -> Bool
keywordMatch keyword searchText =
  String.contains (String.toLower keyword) (String.toLower searchText)


applySearchFilter : Model -> Model
applySearchFilter model =
  let
    { keywords, runewords, searchType } =
      model
  in
    case keywords of
      Nothing ->
        model

      Just keywords ->
        let
          name =
            (\rw -> rw.name)

          runes =
            (\rw -> String.join " " (List.map toString rw.runes))

          properties =
            (\rw -> String.join " " rw.properties)

          all =
            (\rw -> String.join " " [ (name rw), (runes rw), (properties rw) ])

          searchFn =
            case searchType of
              Name ->
                name

              Runes ->
                runes

              Properties ->
                properties

              _ ->
                all

          filtered =
            List.filter (\rw -> List.all (\kw -> keywordMatch kw (searchFn rw)) keywords) runewords
        in
          { model | runewords = filtered }


applySocketFilter : Model -> Model
applySocketFilter model =
  let
    { runewords, minSockets, maxSockets } =
      model

    filtered =
      List.filter (\rw -> socketBounds minSockets maxSockets rw.sockets) runewords
  in
    { model | runewords = filtered }


applyFilters : Model -> Model
applyFilters model =
  model |> applySearchFilter |> applySocketFilter


renderRunewordsList : Model -> Html
renderRunewordsList model =
  let
    { runewords, keywords, searchType } =
      model

    filtered =
      applyFilters model |> .runewords
  in
    div
      [ class "columns" ]
      [ div
          [ class "column" ]
          [ div [] (List.map renderRuneword filtered)
          ]
      ]


type SearchType
  = All
  | Name
  | Runes
  | Properties


stFromString : String -> SearchType
stFromString value =
  case value of
    "All" ->
      All

    "Name" ->
      Name

    "Runes" ->
      Runes

    "Properties" ->
      Properties

    _ ->
      All


renderSearchBar : Signal.Address Action -> Html
renderSearchBar address =
  div
    [ class "control is-horizontal has-addons" ]
    [ div
        [ class "control-label" ]
        [ label [ class "label" ] [ text "Search" ] ]
    , div
        [ class "control" ]
        [ input
            [ class "input is-primary"
            , type' "text"
            , Events.on "input" Events.targetValue (\v -> Signal.message address (KeywordSearch v))
            ]
            []
        , span
            [ class "select" ]
            [ select
                [ Events.on "change" Events.targetValue (\v -> Signal.message address (ChangeSearchType (stFromString v))) ]
                (List.map (\t -> option [] [ text (toString t) ]) [ All, Name, Runes, Properties ])
            ]
        ]
    ]


type SocketType
  = AnySockets
  | Sockets Int


minBounds : SocketType -> Int -> Bool
minBounds min sockets =
  case min of
    AnySockets ->
      True

    Sockets i ->
      sockets >= i


maxBounds : SocketType -> Int -> Bool
maxBounds max sockets =
  case max of
    AnySockets ->
      True

    Sockets i ->
      sockets <= i


socketBounds : SocketType -> SocketType -> Int -> Bool
socketBounds min max sockets =
  minBounds min sockets && maxBounds max sockets


compareSocketType : String -> String -> Order
compareSocketType a b =
  if a == "Any" then
    LT
  else if b == "Any" then
    GT
  else
    compare a b


socketTypes : Dict String SocketType
socketTypes =
  Dict.fromList
    [ ( "Any", AnySockets )
    , ( "2", Sockets 2 )
    , ( "3", Sockets 3 )
    , ( "4", Sockets 4 )
    , ( "5", Sockets 5 )
    , ( "6", Sockets 6 )
    ]


getSocketType : String -> SocketType
getSocketType key =
  case (Dict.get key socketTypes) of
    Just socketType ->
      socketType

    _ ->
      AnySockets


renderSocketFilter : Signal.Address Action -> String -> (SocketType -> Action) -> Html
renderSocketFilter address name action =
  div
    [ class "control is-horizontal" ]
    [ div
        [ class "control-label" ]
        [ label [ class "label" ] [ text name ] ]
    , div
        [ class "control" ]
        [ span
            [ class "select" ]
            [ select
                [ class "input is-secondary"
                , Events.on "change" Events.targetValue (\v -> Signal.message address (action (getSocketType v)))
                ]
                (List.map
                  (\s -> option [] [ text s ])
                  (List.sortWith compareSocketType (Dict.keys socketTypes))
                )
            ]
        ]
    ]


renderSocketFilters : Signal.Address Action -> Html
renderSocketFilters address =
  div
    []
    [ renderSocketFilter address "Min Sockets:" ChangeMinSockets
    , renderSocketFilter address "Max Sockets:" ChangeMaxSockets
    ]


renderFilters : Signal.Address Action -> Html
renderFilters address =
  form
    []
    [ renderSearchBar address
    , renderSocketFilters address
    ]


parseSearchKeywords : String -> Maybe (List String)
parseSearchKeywords query =
  if String.isEmpty query then
    Nothing
  else
    Just (String.split " " query)


renderHero : Html
renderHero =
  section
    [ class "hero" ]
    [ div
        [ class "hero-content" ]
        [ div
            [ class "container" ]
            [ h1
                [ class "title is-1" ]
                [ text "Diablo II Rune Words" ]
            ]
        ]
    ]


renderFooter : Html
renderFooter =
  footer
    [ class "footer" ]
    [ div
        [ class "container" ]
        [ div
            [ class "content is-text-centered" ]
            [ p
                []
                []
            ]
        ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ id "app-body" ]
    [ renderHero
    , section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ div
                [ class "columns" ]
                [ div
                    [ class "column is-half" ]
                    [ renderRunewordsList model ]
                , div
                    [ id "filters", class "column is-half" ]
                    [ renderFilters address ]
                ]
            ]
        ]
    , renderFooter
    ]


update : Action -> Model -> ( Model, Effects Action )
update action model =
  let
    _ =
      Debug.log "action" action

    _ =
      Debug.log "model" model
  in
    case action of
      KeywordSearch query ->
        ( { model | keywords = parseSearchKeywords query }, Effects.none )

      ChangeSearchType searchType ->
        ( { model | searchType = searchType }, Effects.none )

      ChangeMinSockets socketType ->
        ( { model | minSockets = socketType }, Effects.none )

      ChangeMaxSockets socketType ->
        ( { model | maxSockets = socketType }, Effects.none )


app : StartApp.App Model
app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }


main : Signal Html
main =
  app.html


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
