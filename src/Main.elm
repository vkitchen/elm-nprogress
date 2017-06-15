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
  ( { progress = Progress.init, loading = False }, Cmd.none )


type Msg
  = Start
  | Done
  | Loaded
  | SetProgress Progress.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Start ->
      ( { model | loading = True, progress = Progress.init }, Cmd.none)

    Done ->
      ( { model | progress = Progress.done model.progress }, Cmd.none)

    Loaded ->
      ( { model | loading = False }, Cmd.none)

    SetProgress newState ->
      ( { model | progress = newState }
      , Cmd.none
      )


view : Model -> Html Msg
view model =
  div []
    ([ button [ onClick Start ] [ text "Start" ]
    , button [ onClick Done ] [ text "Done" ]
    ] ++ if model.loading then [Progress.view config model.progress] else [])


config : Progress.Config Msg
config =
  Progress.config
    { toMsg = SetProgress
    , toLoaded = Loaded
    }


subscriptions model =
  if model.loading then
    Progress.subscriptions config model.progress
  else
    Sub.none


main =
  program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }
