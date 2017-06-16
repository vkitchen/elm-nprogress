module Progress exposing (Config, State, config, init, start, done, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, millisecond)


(=>) =
  (,)


type State =
  State Bool Bool Float


type Config msg =
  Config
    { toMsg : State -> msg
    , toLoaded : msg
    }


config : { toMsg : State -> msg, toLoaded : msg } -> Config msg
config { toMsg, toLoaded } =
  Config
    { toMsg = toMsg
    , toLoaded = toLoaded
    }


init : State
init =
  State False False 0


start : State
start =
  State True False 0


done : State -> State
done (State _ _ n) =
  State True True n


percent n =
  (n - toFloat (truncate n)) * 100


subscriptions : Config msg -> State -> Sub msg
subscriptions (Config { toMsg, toLoaded }) (State running d n) =
  if running then
    let
      increase _ =
        case d of
          False ->
            toMsg <|
              if n < 0.2 then
                State True False (n + 0.1)
              else if n < 0.5 then
                State True False (n + 0.04)
              else if n < 0.8 then
                State True False (n + 0.02)
              else if n < 0.99 then
                State True False (n + 0.005)
              else
                State True False n
          True ->
            -- at least one extra inverval after the bar finishes
            if n < 2 then
              State True True (n + 0.8) |> toMsg
            else
              toLoaded
    in
    Time.every (millisecond * 200) increase
  else
    Sub.none


view : Config msg -> State -> Html msg
view (Config { toMsg, toLoaded }) (State running d n) =
  if running then
    let
      transform =
          ("translate3d(-" ++ toString (clamp 0 100 (100 - n * 100)) ++ "%, 0px, 0px)")
      barStyle =
        style
          [ "transform" => transform
          , "transition" => "all 200ms linear 0s"
          , "opacity" => if n < 2 then "1" else "0"
          ]
    in
    div [ id "nprogress" ]
      [ div [ class "bar", barStyle ]
          [ div [ class "peg" ]
              []
          ]
      ]
  else
    text ""
