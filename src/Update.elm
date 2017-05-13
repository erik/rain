module Update exposing (Msg(..), update)

import Array
import Date exposing (Date)
import Debug
import Dict
import Dom.Scroll
import Irc
import Model exposing (..)
import Ports
import Task
import Time exposing (Time)
import WebSocket


type Msg
    = AddServer ( ServerName, ServerMetaData )
    | AddLine ServerName ChannelName Line
    | SendLine ServerInfo ChannelInfo String
    | TypeLine String
    | SendRawLine ServerInfo String
    | ReceiveRawLine ServerName String
    | ReceiveLine ( ServerName, Irc.ParsedMessage )
    | CreateChannel ServerName ChannelName
    | SelectChannel ServerName ChannelName
    | CloseChannel ServerName ChannelName
    | RefreshScroll
    | SendNotification String String
    | Tick Time
    | TabCompleteLine ServerInfo ChannelInfo
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddServer ( serverName, metadata ) ->
            let
                -- FIXME: this kind of sucks. maybe there's an extensible
                -- FIXME: record solution?
                info =
                    { socket = metadata.socket
                    , nick = metadata.nick
                    , pass = metadata.pass
                    , name = metadata.name
                    , networkChannel = newChannel metadata.name
                    , channels = Dict.empty
                    }

                -- TODO: add in the other things
                serverInfo_ =
                    model.serverInfo
                        |> Dict.insert serverName info

                model_ =
                    { model | serverInfo = serverInfo_ }
            in
                ( model_, Cmd.none )

        AddLine serverName channelName line ->
            case getServerChannel model ( serverName, channelName ) of
                Just ( serverInfo, chanInfo ) ->
                    let
                        chanInfo_ =
                            { chanInfo | buffer = appendLine chanInfo.buffer line }

                        model_ =
                            setChannel ( serverName, channelName ) chanInfo_ model

                        isChanMsg =
                            String.startsWith "#" channelName

                        cmdNotify =
                            if
                                String.contains serverInfo.nick line.message
                                    || (serverInfo.nick /= line.nick && not isChanMsg)
                            then
                                SendNotification channelName line.message
                            else
                                Noop
                    in
                        update cmdNotify model_

                Nothing ->
                    update (CreateChannel serverName channelName) model
                        |> andThen (AddLine serverName channelName line)

        SendLine serverInfo chanInfo line ->
            let
                privmsg msg =
                    let
                        line =
                            { ts = Date.fromTime model.currentTime
                            , nick = serverInfo.nick
                            , message = msg
                            }

                        nextMsg =
                            AddLine serverInfo.name chanInfo.name line

                        rawLine =
                            String.join " " [ "PRIVMSG", chanInfo.name, ":" ++ msg ]
                    in
                        ( rawLine, nextMsg )

                ( rawLine, nextMsg ) =
                    case String.words line of
                        "/join" :: rest ->
                            ( String.join " " ("JOIN" :: rest), Noop )

                        "/part" :: rest ->
                            ( String.join " " ("PART" :: rest), Noop )

                        "/me" :: rest ->
                            let
                                msg =
                                    String.join " " rest

                                action =
                                    String.join "" [ "\x01", "ACTION ", msg, "\x01" ]
                            in
                                privmsg action

                        "/privmsg" :: rest ->
                            let
                                msg =
                                    String.join " " rest
                            in
                                privmsg msg

                        _ ->
                            if String.startsWith "/" line then
                                ( String.dropLeft 1 line, Noop )
                            else
                                privmsg line

                model_ =
                    { model | inputLine = "" }
            in
                update (SendRawLine serverInfo rawLine) model_
                    |> andThen nextMsg
                    |> andThen RefreshScroll

        TypeLine str ->
            let
                model_ =
                    { model | inputLine = str }
            in
                ( model_, Cmd.none )

        SendRawLine serverInfo line ->
            ( model, WebSocket.send serverInfo.socket line )

        ReceiveRawLine serverName line ->
            ( model, Ports.parse_raw ( serverName, line ) )

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
                    let
                        model_ =
                            { model | current = Just ( serverName, channelName ) }
                    in
                        update RefreshScroll model_

                _ ->
                    -- TODO: handle this?
                    Debug.log "tried to select a bad channel?"
                        ( model, Cmd.none )

        RefreshScroll ->
            ( model, Task.attempt (\_ -> Noop) (Dom.Scroll.toBottom "body") )

        SendNotification title message ->
            ( model, Ports.send_notification ( title, message ) )

        TabCompleteLine serverInfo channelInfo ->
            let
                words =
                    String.words model.inputLine

                lastWord =
                    case List.reverse words of
                        word :: _ ->
                            if word == "" then
                                Nothing
                            else
                                Just word

                        _ ->
                            Nothing

                -- TODO: should also complete /privmsg etc
                completions =
                    lastWord
                        |> Maybe.map
                            (\w ->
                                channelInfo.users
                                    |> Dict.values
                                    |> List.filter (\u -> String.startsWith w u.nick)
                                    |> List.map (.nick)
                            )

                longestCompletion =
                    completions
                        |> Maybe.map List.sort
                        |> Maybe.andThen List.head
            in
                case longestCompletion of
                    Just c ->
                        let
                            words_ =
                                List.drop 1 words
                                    |> String.join " "

                            completion =
                                c ++ ": "

                            newInput =
                                String.join " " [ words_, completion ]
                        in
                            -- TODO: handle nick / command completion for real
                            ( { model | inputLine = String.trimLeft newInput }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        Tick time ->
            ( { model | currentTime = time }, Cmd.none )

        _ ->
            -- TODO: handle these cases
            ( model, Cmd.none )


{-| Nabbed from <http://stackoverflow.com/a/42050830>
-}
andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            update msg model
    in
        newModel ! [ cmd, newCmd ]


handleMessage : ServerName -> Irc.Message -> Date -> Model -> ( Model, Cmd Msg )
handleMessage serverName parsedMsg date model =
    case getServer model serverName of
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
                            { chanInfo | users = Dict.insert who.nick user chanInfo.users }

                        model_ =
                            setChannel serverChan chanInfo_ model
                    in
                        ( { model_ | current = Just serverChan }, Cmd.none )

                Irc.Privmsg { from, target, text } ->
                    let
                        newLine =
                            { ts = date, nick = from.nick, message = text }

                        newMsg =
                            AddLine serverName target newLine
                    in
                        update newMsg model
                            |> andThen RefreshScroll

                Irc.TopicIs { channel, text } ->
                    let
                        chanInfo =
                            getOrCreateChannel model ( serverName, channel )

                        chanInfo_ =
                            { chanInfo | topic = Just text }
                    in
                        ( setChannel ( serverName, channel ) chanInfo_ model, Cmd.none )

                Irc.Nick { who, nick } ->
                    let
                        server_ =
                            if who.nick == serverInfo.nick then
                                { serverInfo | nick = nick }
                            else
                                serverInfo

                        model_ =
                            { model | serverInfo = Dict.insert serverName server_ model.serverInfo }
                    in
                        ( model_, Cmd.none )

                Irc.NickList { channel, users } ->
                    let
                        chanInfo =
                            getOrCreateChannel model ( serverName, channel )

                        userDict =
                            users
                                |> List.map (\u -> ( u.nick, u ))
                                |> Dict.fromList
                                |> Dict.union chanInfo.users

                        chanInfo_ =
                            { chanInfo | users = userDict }

                        model_ =
                            setChannel ( serverName, channel ) chanInfo_ model
                    in
                        ( model_, Cmd.none )

                Irc.Unknown msg ->
                    let
                        msgText =
                            msg.params
                                |> Array.toList
                                |> List.drop 1
                                |> String.join " "

                        newLine =
                            { ts = date
                            , nick = msg.prefix
                            , message = String.join ": " [ msg.command, msgText ]
                            }

                        newMsg =
                            AddLine serverName serverBufferName newLine

                        _ =
                            Debug.log "unknown msg" msg
                    in
                        update newMsg model
                            |> andThen RefreshScroll

                msg ->
                    let
                        _ =
                            Debug.log "unhandled message type" msg
                    in
                        ( model, Cmd.none )

        Nothing ->
            Debug.log "getServer was none?" ( model, Cmd.none )
