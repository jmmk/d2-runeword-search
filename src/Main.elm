module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (id, class, style)
import Html.Events as Events
import Effects exposing (Effects, Never)
import Task
import String
import Runewords exposing (Runeword, Patch(..), runewords)


type alias Model =
  { keywords : Maybe (List String)
  , runewords : List Runeword
  , searchType : SearchType
  }


initialModel : Model
initialModel =
  { keywords = Nothing
  , runewords = runewords
  , searchType = All
  }


init : ( Model, Effects Action )
init =
  ( initialModel, Effects.none )


type Action
  = KeywordSearch String
  | ChangeSearchType SearchType


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


filterRunewords : List String -> List Runeword -> SearchType -> List Runeword
filterRunewords keywords runewords searchType =
  let
    searchText =
      case searchType of
        Name ->
          (\runeword -> runeword.name)

        Runes ->
          (\runeword -> String.join "" (List.map toString runeword.runes))

        Properties ->
          (\runeword -> String.join " " runeword.properties)

        _ ->
          (\runeword -> runeword.name)
  in
    List.filter (\rw -> List.all (\kw -> keywordMatch kw (searchText rw)) keywords) runewords


renderRunewordsList : Model -> Html.Html
renderRunewordsList model =
  let
    { runewords, keywords, searchType } =
      model

    filtered =
      case keywords of
        Nothing ->
          runewords

        Just keywords ->
          filterRunewords keywords runewords searchType
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
        [ label [ class "label" ] [ text "Search: " ]
        ]
    , div
        [ class "control" ]
        [ input [ class "input", Events.on "input" Events.targetValue (\v -> Signal.message address (KeywordSearch v)) ] []
        , span
            [ class "select" ]
            [ select
                [ Events.on "change" Events.targetValue (\v -> Signal.message address (ChangeSearchType (stFromString v))) ]
                (List.map (\t -> option [] [ text (toString t) ]) [ All, Name, Runes, Properties ])
            ]
        ]
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
                [ text "Made with love for Beti" ]
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
                    [ class "column is-half" ]
                    [ renderSearchBar address ]
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


app : StartApp.App Model
app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }


main : Signal Html.Html
main =
  app.html


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
