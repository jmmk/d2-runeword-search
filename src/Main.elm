module Main (..) where

import StartApp
import Html exposing (div, p, h1, h2, h3, text, label, li, ul, input, header, span, section, header, footer)
import Html.Attributes exposing (id, class)
import Html.Events as Events
import Effects exposing (Effects, Never)
import Task
import String
import Runewords exposing (Runeword, runewords)


type alias Model =
  { keywords : Maybe (List String)
  , runewords : List Runeword
  }


initialModel : Model
initialModel =
  { keywords = Nothing
  , runewords = runewords
  }


init : ( Model, Effects Action )
init =
  ( initialModel, Effects.none )


type Action
  = KeywordSearch String


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
      -- , h3 [] [ text ("Type: " ++ (toString runeword.itemType)) ]
      -- , h3 [] [ text ("Weapon Type: " ++ (toString runeword.weaponType)) ]
    , h3 [] [ text ("Sockets: " ++ (toString runeword.sockets)) ]
    , h3 [] [ text ("Ladder Only: " ++ (toString runeword.ladderOnly)) ]
    ]


keywordMatch : String -> Runeword -> Bool
keywordMatch keyword runeword =
  String.contains (String.toLower keyword) (String.toLower runeword.name)


filterRunewords : List String -> List Runeword -> List Runeword
filterRunewords keywords runewords =
  List.filter (\r -> List.all (\kw -> keywordMatch kw r) keywords) runewords


renderRunewordsList : Maybe (List String) -> List Runeword -> Html.Html
renderRunewordsList keywords runewords =
  let
    filtered =
      case keywords of
        Nothing ->
          runewords

        Just keywords ->
          filterRunewords keywords runewords
  in
    div
      [ class "columns" ]
      [ div
          [ class "column" ]
          [ div [] (List.map renderRuneword filtered)
          ]
      ]


renderSearchField : Signal.Address Action -> Html.Html
renderSearchField address =
  div
    [ class "control is-horizontal" ]
    [ div
        [ class "control-label" ]
        [ label [ class "label" ] [ text "Search: " ]
        ]
    , div
        [ class "control" ]
        [ input [ class "input", Events.on "input" Events.targetValue (\v -> Signal.message address (KeywordSearch v)) ] []
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
                    [ renderRunewordsList model.keywords model.runewords ]
                , div
                    [ class "column is-half" ]
                    [ renderSearchField address ]
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


app : StartApp.App Model
app =
  StartApp.start { init = init, update = update, view = view, inputs = [] }


main : Signal Html.Html
main =
  app.html


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
