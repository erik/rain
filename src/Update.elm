module Update exposing (Msg(..), update)

import Debug
import Dict as D
import WebSocket

import Irc
import Model exposing (..)


type Msg
  = SendLine
  | TypeLine String
  | SendRawLine ServerName String
  | ReceiveRawLine  ServerName String
  | ReceiveLine ( ServerName, Irc.ParsedMessage )
  | CreateChannel ServerName ChannelName
  | SelectChannel ServerName ChannelName
  | CloseChannel ServerName ChannelName
  | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendLine ->
            -- TODO: need to implement this
            ( model, Cmd.none )

        TypeLine str ->
            case getChannel model.current model of
                Just chan ->
                    let
                        channel_ = { chan | inputLine = str }
                        model_ = setChannel model.current channel_ model
                    in
                        ( model_, Cmd.none )

                Nothing ->
                    -- TODO: handle this?
                    Debug.log "getChannel was none?"
                    ( model, Cmd.none )


        SendRawLine serverName line ->
            case getServer model.current model of
                Just server ->
                    ( model, WebSocket.send server.socket line )
                Nothing ->
                    Debug.log "getServer was none?"
                    ( model, Cmd.none )


        ReceiveRawLine serverName line ->
            ( model, Irc.parse_raw (serverName, line) )

        CreateChannel serverName channelName ->
            let
                channel = Model.newChannel
                model_ = setChannel ( serverName, channelName ) channel model
            in
                ( model_, Cmd.none )

        SelectChannel serverName channelName ->
            case getChannel (serverName, channelName) model of
                Just _ ->
                    ( { model | current = ( serverName, channelName ) }, Cmd.none )
                _ ->
                    -- TODO: handle this?
                    Debug.log "tried to select a bad channel?"
                    ( model, Cmd.none )

        _ ->
          -- TODO: handle these cases
          ( model, Cmd.none )


handleMessage : ( ServerName, Irc.Message ) -> Model -> ( Model, Cmd Msg )
handleMessage (serverName, parsedMsg) model =
    case getServer ( serverName, "" ) model of
        Just s ->
            case parsedMsg of
                Irc.Ping s ->
                    update ( SendRawLine serverName ("PONG " ++ s) ) model
                Irc.Joined { who, channel } ->
                    let
                        user = { nick = who.nick
                               , user = who.realname
                               , host = who.hostname
                               , name = who.realname
                               }

                        chanInfo = getChannel ( serverName, channel ) model
                                 |> Maybe.withDefault Model.newChannel

                        chanInfo_ = { chanInfo | users = D.insert who.nick user chanInfo.users }
                    in

                        ( setChannel ( serverName, channel) chanInfo_ model, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        Nothing ->
            Debug.log "getServer was none?"
            ( model, Cmd.none )
