port module Main exposing (..)

import Date
import Date.Extra.Format as Format

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

import Irc exposing (Message)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

echoServer : String
echoServer = "ws://localhost:8080/"

-- Model

type alias Model =
  { input : String
  , messages : List (Date.Date, Irc.Message)
  }


initialModel : Model
initialModel = { input = "" , messages = [] }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- Update

type Msg
  = Input String
  | SendMessage
  | RecvMessage Irc.ParsedMessage
  | RecvWebSocket String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
          ( { model | input = newInput }, Cmd.none )
        SendMessage ->
          ( { model | input = "" }, WebSocket.send echoServer model.input )
        RecvMessage m ->
          let parsed = Irc.parse m
          in ( { model | messages = parsed :: model.messages}, Cmd.none )
        RecvWebSocket str ->
          ( model, parse_irc str )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ WebSocket.listen echoServer RecvWebSocket
    , irc_messages RecvMessage
    ]

-- Ports

port parse_irc : String -> Cmd msg
port irc_messages : (Irc.ParsedMessage -> msg) -> Sub msg

-- View

view : Model -> Html Msg
view model =
  div []
    [ input [onInput Input, value model.input] []
    , button [onClick SendMessage] [text "Send"]
    , div [] (List.map viewMessage (List.reverse model.messages))
    ]

viewMessage : (Date.Date, Irc.Message) -> Html Msg
viewMessage (ts, msg) =
  let
    innerHtml =
      case msg of
          Irc.Unknown s ->
            text s.raw
          Irc.Privmsg {from, target, text} ->
            div [] [
               pre [] [ Html.text <| target ++ ": <" ++ from.nick ++ ">: " ++ text]
              ]
          _ ->
            text "something happened"
  in
      div []
        [
         div [] [ pre [] [ text <| "at " ++ (Format.isoString ts) ] ]
        , innerHtml
        ]
