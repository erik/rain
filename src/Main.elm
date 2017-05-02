module Main exposing (..)

import Date
import Date.Extra.Format as Format

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

import Irc exposing (Message)

import Model exposing (Model, initialModel)
import Update exposing (update, Msg(..))
import View exposing (view)

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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Irc.irc_messages ReceiveLine
    ]
