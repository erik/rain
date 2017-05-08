module Update exposing (Msg(..), update)

import Date exposing (Date)
import Debug
import Dict as D
import Dom.Scroll
import Irc
import Model exposing (..)
import Task
import WebSocket


type Msg
    = AddServer ( ServerName, ServerInfo )
    | SendLine ServerInfo ChannelInfo String
    | TypeLine String
    | SendRawLine ServerInfo String
    | ReceiveRawLine ServerName String
    | ReceiveLine ( ServerName, Irc.ParsedMessage )
    | CreateChannel ServerName ChannelName
    | SelectChannel ServerName ChannelName
    | CloseChannel ServerName ChannelName
    | RefreshScroll
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddServer ( serverName, info ) ->
            let
                serverInfo_ =
                    model.serverInfo
                        |> D.insert serverName info

                model_ =
                    { model | serverInfo = serverInfo_ }
            in
                ( model_, Cmd.none )

        SendLine serverInfo chanInfo line ->
            let
                rawLine =
                    case String.split " " line of
                        "/join" :: rest ->
                            String.join " " ("JOIN" :: rest)

                        "/part" :: rest ->
                            String.join " " ("PART" :: rest)

                        "/privmsg" :: rest ->
                            String.join " " ("PRIVMSG" :: rest)

                        _ ->
                            String.join " " [ "PRIVMSG", chanInfo.name, line ]

                model_ =
                    { model | inputLine = "" }
            in
                update (SendRawLine serverInfo rawLine) model_

        TypeLine str ->
            let
                model_ =
                    { model | inputLine = str }
            in
                ( model_, Cmd.none )

        SendRawLine serverInfo line ->
            ( model, WebSocket.send serverInfo.socket line )

        ReceiveRawLine serverName line ->
            ( model, Irc.parse_raw ( serverName, line ) )

        ReceiveLine ( serverName, parsed ) ->
            let
                -- FIXME: lol
                ( ts, ircMsg ) =
                    Irc.parse parsed
            in
                handleMessage serverName ircMsg ts model

        CreateChannel serverName channelName ->
            let
                channel =
                    Model.newChannel channelName

                model_ =
                    setChannel ( serverName, channelName ) channel model
            in
                ( model_, Cmd.none )

        SelectChannel serverName channelName ->
            case getChannel model ( serverName, channelName ) of
                Just _ ->
                    ( { model | current = Just ( serverName, channelName ) }, Cmd.none )

                _ ->
                    -- TODO: handle this?
                    Debug.log "tried to select a bad channel?"
                        ( model, Cmd.none )

        RefreshScroll ->
            ( model, Task.attempt (\_ -> Noop) (Dom.Scroll.toBottom "buffer-view") )

        _ ->
            -- TODO: handle these cases
            ( model, Cmd.none )


handleMessage : ServerName -> Irc.Message -> Date -> Model -> ( Model, Cmd Msg )
handleMessage serverName parsedMsg date model =
    case getServer model ( serverName, "" ) of
        Just serverInfo ->
            case parsedMsg of
                Irc.Ping x ->
                    update (SendRawLine serverInfo ("PONG " ++ x)) model

                Irc.Joined { who, channel } ->
                    let
                        user =
                            { nick = who.nick
                            , user = who.realname
                            , host = who.hostname
                            , name = who.realname
                            }

                        serverChan =
                            ( serverName, channel )

                        chanInfo =
                            getChannel model serverChan
                                |> Maybe.withDefault (Model.newChannel channel)

                        chanInfo_ =
                            { chanInfo | users = D.insert who.nick user chanInfo.users }

                        model_ =
                            setChannel serverChan chanInfo_ model
                    in
                        ( { model_ | current = Just serverChan }, Cmd.none )

                Irc.Privmsg { from, target, text } ->
                    let
                        chanInfo =
                            getOrCreateChannel model ( serverName, target )

                        newLine =
                            { ts = date, nick = from.nick, message = text }

                        chanInfo_ =
                            { chanInfo | buffer = appendLine chanInfo.buffer newLine }

                        model_ =
                            setChannel ( serverName, target ) chanInfo_ model
                    in
                        update RefreshScroll model_

                Irc.TopicIs { channel, text } ->
                    let
                        chanInfo =
                            getOrCreateChannel model ( serverName, channel )

                        _ =
                            Debug.log <| channel ++ "topic is" ++ text

                        chanInfo_ =
                            { chanInfo | topic = Just text }
                    in
                        ( setChannel ( serverName, channel ) chanInfo_ model, Cmd.none )

                msg ->
                    let
                        _ =
                            Debug.log "unknown msg" msg
                    in
                        ( model, Cmd.none )

        Nothing ->
            Debug.log "getServer was none?" ( model, Cmd.none )
