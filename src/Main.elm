module Main exposing (..)

import Dict
import Html exposing (..)
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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- Establish all of our open websocket connections
        recvWs =
            model.serverInfo
                |> Dict.toList
                |> List.map
                    (\( serverName, info ) ->
                        WebSocket.listen info.socket
                            (\n -> ReceiveRawLine serverName n)
                    )
    in
        Sub.batch
            ([ Irc.irc_messages ReceiveLine
             , Model.saved_servers AddServer
             ]
                ++ recvWs
            )
