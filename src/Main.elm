module Main exposing (main)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Progress
import Time exposing (Time, millisecond)


(=>) =
  (,)


type alias Model =
  { progress : Progress.State
  , loading : Bool
  }


init =
  ( { progress = Progress.init, loading = True }, Cmd.none )


type Msg
  = Start
  | Done
  | Loaded
  | SetProgress Progress.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Start ->
      ( { model | progress = Progress.start }, Cmd.none)

    Done ->
      ( { model | progress = Progress.done model.progress }, Cmd.none)

    Loaded ->
      ( { model | progress = Progress.init, loading = False }, Cmd.none)

    SetProgress newState ->
      ( { model | progress = newState }
      , Cmd.none
      )


view : Model -> Html Msg
view model =
  div []
    [ Progress.view config model.progress
    , button [ onClick Start ] [ text "Start" ]
    , button [ onClick Done ] [ text "Done" ]
    , if model.loading then
        text ""
      else
        text "Page has loaded"
    ]


config : Progress.Config Msg
config =
  Progress.config
    { toMsg = SetProgress
    , toLoaded = Loaded
    }


subscriptions model =
  Progress.subscriptions config model.progress


main =
  program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }
