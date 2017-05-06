module Update exposing (Msg(..), update)

import Date exposing (Date)
import Debug
import Dict as D
import Irc
import Model exposing (..)
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
            in
                update (SendRawLine serverInfo rawLine) model

        TypeLine str ->
            case ( model.current, getActiveChannel model ) of
                ( Just current, Just chan ) ->
                    let
                        channel_ =
                            { chan | inputLine = str }

                        model_ =
                            setChannel current channel_ model
                    in
                        ( model_, Cmd.none )

                _ ->
                    -- TODO: handle this?
                    Debug.log "getChannel was none?"
                        ( model, Cmd.none )

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
                            { chanInfo | buffer = (newLine :: chanInfo.buffer) }
                    in
                        ( setChannel ( serverName, target ) chanInfo_ model, Cmd.none )

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
