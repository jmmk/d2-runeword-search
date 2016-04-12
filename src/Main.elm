module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (id, class, style, type')
import Html.Events as Events
import Effects exposing (Effects, Never)
import Task
import String
import Runewords exposing (Runeword, Patch(..), runewords)


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


renderProperties : List String -> Html.Html
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


renderAttributes : Runeword -> Html.Html
renderAttributes runeword =
  div
    [ class "content" ]
    [ h6 [] [ text ("Sockets: " ++ (toString runeword.sockets)) ]
    , h6 [] [ text ("Ladder Only: " ++ (toString runeword.ladderOnly)) ]
    , h6 [] [ text ("Patch: " ++ (showPatch runeword.patch)) ]
    , h6 [] [ text ("Required Clvl: " ++ (toString runeword.clvl)) ]
    ]


renderRuneword : Runeword -> Html.Html
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
      List.filter (\rw -> compareSocketType rw.sockets minSockets GT && compareSocketType rw.sockets maxSockets LT) runewords
  in
    { model | runewords = filtered }


applyFilters : Model -> Model
applyFilters model =
  model |> applySearchFilter |> applySocketFilter


renderRunewordsList : Model -> Html.Html
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


renderSearchBar : Signal.Address Action -> Html.Html
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
  | Two
  | Three
  | Four
  | Five
  | Six


socketTypeToString : SocketType -> String
socketTypeToString socketType =
  case socketType of
    AnySockets ->
      "Any"

    Two ->
      "2"

    Three ->
      "3"

    Four ->
      "4"

    Five ->
      "5"

    Six ->
      "6"


compareSocketType : Int -> SocketType -> Order -> Bool
compareSocketType sockets socketType ord =
  case ord of
    GT ->
      case socketType of
        AnySockets ->
          True

        Two ->
          sockets >= 2

        Three ->
          sockets >= 3

        Four ->
          sockets >= 4

        Five ->
          sockets >= 5

        Six ->
          sockets >= 6

    _ ->
      case socketType of
        AnySockets ->
          True

        Two ->
          sockets <= 2

        Three ->
          sockets <= 3

        Four ->
          sockets <= 4

        Five ->
          sockets <= 5

        Six ->
          sockets <= 6


renderSocketFilter : Signal.Address Action -> String -> (SocketType -> Action) -> Html.Html
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
                [ class "input is-secondary" ]
                (List.map (\s -> option [] [ text (socketTypeToString s) ]) [ AnySockets, Two, Three, Four, Five, Six ])
            ]
        ]
    ]


renderSocketFilters : Signal.Address Action -> Html.Html
renderSocketFilters address =
  div
    []
    [ renderSocketFilter address "Min Sockets:" ChangeMinSockets
    , renderSocketFilter address "Max Sockets:" ChangeMaxSockets
    ]


renderFilters : Signal.Address Action -> Html.Html
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


renderHero : Html.Html
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


renderFooter : Html.Html
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


view : Signal.Address Action -> Model -> Html.Html
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


main : Signal Html.Html
main =
  app.html


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
