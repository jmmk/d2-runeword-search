module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (id, class, style, type')
import Html.Events as Events
import Effects exposing (Effects, Never)
import Task
import String
import Runewords exposing (Runeword, runewords)
import Dict exposing (Dict)
import Patch exposing (Patch)
import Sockets exposing (Sockets)
import SearchType exposing (SearchType)


type alias Model =
  { keywords : Maybe (List String)
  , runewords : List Runeword
  , searchType : SearchType
  , minSockets : Sockets
  , maxSockets : Sockets
  }


initialModel : Model
initialModel =
  { keywords = Nothing
  , runewords = runewords
  , searchType = SearchType.All
  , minSockets = Sockets.AnySockets
  , maxSockets = Sockets.AnySockets
  }


init : ( Model, Effects Action )
init =
  ( initialModel, Effects.none )


type Action
  = KeywordSearch String
  | ChangeSearchType SearchType
  | ChangeMinSockets Sockets
  | ChangeMaxSockets Sockets


renderProperties : List String -> Html
renderProperties properties =
  div
    [ class "content" ]
    [ ul
        [ style [ ( "margin-top", "0" ) ] ]
        (List.map (\p -> li [] [ text p ]) properties)
    ]


renderAttributes : Runeword -> Html
renderAttributes runeword =
  div
    [ class "content" ]
    [ h6 [] [ text ("Sockets: " ++ (toString runeword.sockets)) ]
    , h6 [] [ text ("Ladder Only: " ++ (toString runeword.ladderOnly)) ]
    , h6 [] [ text ("Patch: " ++ (Patch.toString runeword.patch)) ]
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
              SearchType.Name ->
                name

              SearchType.Runes ->
                runes

              SearchType.Properties ->
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
      List.filter (\rw -> Sockets.socketBounds minSockets maxSockets rw.sockets) runewords
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


renderSearchBar : Signal.Address Action -> Html
renderSearchBar address =
  div
    [ class "control" ]
    [ label [ class "label" ] [ text "Search" ]
    , div
        [ class "control has-addons" ]
        [ input
            [ class "input is-primary"
            , type' "text"
            , Events.on "input" Events.targetValue (\v -> Signal.message address (KeywordSearch v))
            ]
            []
        , span
            [ class "select" ]
            [ select
                [ Events.on "change" Events.targetValue (\v -> Signal.message address (ChangeSearchType (SearchType.fromString v))) ]
                (List.map (\t -> option [] [ text t ]) SearchType.displayList)
            ]
        ]
    ]


renderSocketFilter : Signal.Address Action -> (Sockets -> Action) -> Html
renderSocketFilter address action =
  span
    [ class "select" ]
    [ select
        [ class "input is-secondary"
        , Events.on "change" Events.targetValue (\v -> Signal.message address (action (Sockets.fromString v)))
        ]
        (List.map
          (\s -> option [] [ text s ])
          (Sockets.displayList)
        )
    ]


renderSocketFilters : Signal.Address Action -> Html
renderSocketFilters address =
  div
    [ class "control" ]
    [ label
        [ class "label" ]
        [ text "Sockets" ]
    , div
        [ class "control is-horizontal" ]
        [ div [ class "control-label" ] [ label [ class "label" ] [ text "Min" ] ]
        , div [ class "control" ] [ renderSocketFilter address ChangeMinSockets ]
        , div [ class "control-label" ] [ label [ class "label" ] [ text "Max" ] ]
        , div [ class "control" ] [ renderSocketFilter address ChangeMaxSockets ]
        ]
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
