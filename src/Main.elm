module Main exposing (..)

import Dict
import Html exposing (..)
import Model exposing (Model, initialModel, getServer)
import Ports
import Time
import Update exposing (update, Msg(..), ServerMsg(..))
import View exposing (view)
import WebSocket


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
            model.servers
                |> Dict.values
                |> List.map
                    (\info ->
                        WebSocket.listen info.socket
                            (\lines ->
                                lines
                                    |> String.trim
                                    |> String.lines
                                    |> List.filter (not << String.isEmpty)
                                    |> List.map (ReceiveRawLine info.meta.name)
                                    |> MultiMsg
                            )
                    )
    in
        Sub.batch
            (List.append
                [ Ports.addSavedServer AddServer
                , Ports.receiveScrollback
                    (\( server, chan, line ) ->
                        case getServer model server of
                            Just serverInfo ->
                                AddLine chan line |> ModifyServer server

                            Nothing ->
                                Noop
                    )
                , Time.every Time.second Tick
                ]
                recvWs
            )
