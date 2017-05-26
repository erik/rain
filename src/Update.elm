module Update exposing (Msg(..), update)

import Array
import Date exposing (Date)
import Debug
import Dict
import Form exposing (Form)
import Irc
import Model exposing (..)
import Ports
import Regex
import Set
import Time exposing (Time)
import WebSocket


type Msg
    = AddServer ServerMetaData
    | AddLine ServerName ChannelName Line
    | CloseChannel ServerName ChannelName
    | ConnectIrc ServerName
    | CreateChannel ServerName ChannelName
    | FormMsg Form.Msg
    | ReceiveLine ( ServerName, Irc.ParsedMessage )
    | ReceiveRawLine ServerName String
    | RefreshScroll Bool
    | SelectChannel ServerName ChannelName
    | SendLine ServerInfo ChannelInfo String
    | SendNotification String String
    | SendRawLine ServerInfo String
    | ShowAddServerForm
    | TabCompleteLine ServerInfo ChannelInfo
    | Tick Time
    | TypeLine String
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddServer meta ->
            let
                -- We send meta.name to differentiate the query
                -- strings so elm opens up multiple websockets
                queryString =
                    [ ( "host", meta.server )
                    , ( "port", meta.port_ )
                    , ( "proxyPass", meta.proxyPass )
                    , ( "name", meta.name )
                    ]
                        |> List.map (\( k, v ) -> k ++ "=" ++ v)
                        |> String.join "&"

                socketUrl =
                    String.join "" [ meta.proxyHost, "?", queryString ]

                networkChannel =
                    newChannel meta.name
                        |> \chan -> { chan | isServer = True }

                pass =
                    -- TODO: meta.pass should be a maybe in the first place.
                    if meta.pass == "" then
                        Nothing
                    else
                        Just meta.pass

                info =
                    { socket = socketUrl
                    , nick = meta.nick
                    , pass = pass
                    , name = meta.name
                    , networkChannel = networkChannel
                    , channels = Dict.empty
                    }

                serverInfo_ =
                    model.serverInfo
                        |> Dict.insert meta.name info

                model_ =
                    { model | serverInfo = serverInfo_ }
            in
                ( model_, Cmd.none )

        ConnectIrc serverName ->
            case getServer model serverName of
                Just server ->
                    let
                        passMsg =
                            server.pass
                                |> Maybe.map (\pass -> "PASS " ++ pass)
                                |> Maybe.withDefault ""

                        lines =
                            [ passMsg
                            , "CAP REQ znc.in/server-time-iso"
                            , "CAP REQ server-time"
                            , "CAP END"
                            , "NICK " ++ server.nick
                            , "USER " ++ server.nick ++ " * * :" ++ server.nick
                            ]

                        connectionMessages =
                            List.map (SendRawLine server) lines
                    in
                        List.foldr (andThen) ( model, Cmd.none ) connectionMessages

                Nothing ->
                    Debug.crash ("Bad servername given" ++ serverName)

        AddLine serverName channelName line ->
            case getServerChannel model ( serverName, channelName ) of
                Just ( serverInfo, chanInfo ) ->
                    let
                        chanInfo_ =
                            { chanInfo | buffer = appendLine chanInfo.buffer line }

                        model_ =
                            setChannel ( serverName, channelName ) chanInfo_ model

                        nickRegexp =
                            Regex.regex ("\\b" ++ serverInfo.nick ++ "\\b")

                        matchesNick =
                            Regex.contains nickRegexp line.message

                        isDirectMessage =
                            (serverInfo.nick /= line.nick)
                                && (not (String.startsWith "#" channelName))

                        body =
                            String.join "" [ "<", line.nick, ">: ", line.message ]

                        cmdNotify =
                            if matchesNick || (isDirectMessage && channelName /= serverBufferName) then
                                SendNotification chanInfo.name body
                            else
                                Noop
                    in
                        update cmdNotify model_

                Nothing ->
                    update (CreateChannel serverName channelName) model
                        |> andThen (AddLine serverName channelName line)

        SendLine serverInfo chanInfo line ->
            let
                privmsg target msg =
                    let
                        line =
                            { ts = Date.fromTime model.currentTime
                            , nick = serverInfo.nick
                            , message = msg
                            }

                        nextMsg =
                            AddLine serverInfo.name target line

                        rawLine =
                            String.join " " [ "PRIVMSG", target, ":" ++ msg ]
                    in
                        ( rawLine, nextMsg )

                ( rawLine, nextMsg ) =
                    case String.words line of
                        [ "/join", channel ] ->
                            ( String.join " " [ "JOIN", channel ], Noop )

                        [ "/part" ] ->
                            ( "PART " ++ chanInfo.name, Noop )

                        [ "/part", channel ] ->
                            ( String.join " " [ "PART", channel ], Noop )

                        "/me" :: rest ->
                            let
                                msg =
                                    String.join " " rest

                                action =
                                    String.join "" [ "\x01", "ACTION ", msg, "\x01" ]
                            in
                                privmsg chanInfo.name action

                        "/msg" :: target :: rest ->
                            let
                                msg =
                                    String.join " " rest
                            in
                                privmsg target msg

                        "/quote" :: rest ->
                            let
                                msg =
                                    String.join " " rest
                            in
                                ( msg, Noop )

                        _ ->
                            if String.startsWith "/" line then
                                ( String.dropLeft 1 line, Noop )
                            else
                                privmsg chanInfo.name line

                model_ =
                    { model | inputLine = "" }
            in
                if model.inputLine == "" then
                    ( model, Cmd.none )
                else
                    update (SendRawLine serverInfo rawLine) model_
                        |> andThen nextMsg
                        |> andThen (RefreshScroll True)

        TypeLine str ->
            let
                model_ =
                    { model | inputLine = str }
            in
                ( model_, Cmd.none )

        SendRawLine serverInfo line ->
            let
                _ =
                    Debug.log "sending" line
            in
                ( model, WebSocket.send serverInfo.socket line )

        ReceiveRawLine serverName line ->
            ( model, Ports.parseRawLine ( serverName, line ) )

        ReceiveLine ( serverName, parsed ) ->
            let
                -- FIXME: it's really silly that this works like this
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
            let
                -- FIXME: ugly naming
                channel =
                    getOrCreateChannel model ( serverName, channelName )
                        |> \ch -> { ch | lastChecked = model.currentTime }

                model_ =
                    setChannel ( serverName, channelName ) channel model
                        |> \model ->
                            { model
                                | current = Just ( serverName, channelName )
                                , newServerForm = Nothing
                            }
            in
                update (RefreshScroll True) model_

        RefreshScroll force ->
            ( model, Ports.refreshScrollPosition force )

        SendNotification title message ->
            ( model, Ports.sendNotification ( title, message ) )

        TabCompleteLine serverInfo channelInfo ->
            let
                words =
                    String.words model.inputLine

                lastWord =
                    List.reverse words
                        |> List.filter (\w -> not (String.isEmpty w))
                        |> List.head

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
                    Just completion ->
                        let
                            newInput =
                                case words of
                                    [ word ] ->
                                        completion ++ ": "

                                    words ->
                                        [ completion ]
                                            |> List.append (List.take ((List.length words) - 1) words)
                                            |> String.join " "
                        in
                            ( { model | inputLine = String.trimLeft newInput }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        Tick time ->
            let
                model_ =
                    case getActive model of
                        Just ( serverInfo, chanInfo ) ->
                            setChannel ( serverInfo.name, chanInfo.name )
                                { chanInfo | lastChecked = time }
                                model

                        Nothing ->
                            model
            in
                ( { model_ | currentTime = time }, Cmd.none )

        FormMsg formMsg ->
            let
                form_ =
                    model.newServerForm

                serverMeta =
                    form_
                        |> Maybe.andThen (Form.getOutput)
            in
                case ( formMsg, serverMeta ) of
                    ( Form.Submit, Just serverMeta ) ->
                        update (AddServer serverMeta) { model | newServerForm = Nothing }

                    _ ->
                        -- FIXME: gnarly.
                        ( { model
                            | newServerForm =
                                form_
                                    |> Maybe.map (Form.update newServerValidation formMsg)
                          }
                        , Cmd.none
                        )

        ShowAddServerForm ->
            let
                form =
                    Form.initial [] newServerValidation
            in
                ( { model | newServerForm = Just form }, Cmd.none )

        CloseChannel serverName channelName ->
            -- TODO: implement me.
            ( model, Cmd.none )

        Noop ->
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


ignoredCommands : Set.Set String
ignoredCommands =
    Set.fromList [ "333", "366" ]


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

                        current_ =
                            if serverInfo.nick == who.nick then
                                Just serverChan
                            else
                                model.current
                    in
                        ( { model_ | current = current_ }, Cmd.none )

                Irc.Privmsg { from, target, text, notice } ->
                    let
                        -- FIXME: this is a hack.
                        noticeText =
                            if notice then
                                "NOTICE: " ++ text
                            else
                                text

                        newLine =
                            { ts = date, nick = from.nick, message = noticeText }

                        newMsg =
                            AddLine serverName target newLine

                        refreshMsg =
                            if Just ( serverInfo.name, target ) == model.current then
                                RefreshScroll False
                            else
                                Noop
                    in
                        update newMsg model
                            |> andThen refreshMsg

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
                        if Set.member msg.command ignoredCommands then
                            ( model, Cmd.none )
                        else
                            update newMsg model
                                |> andThen (RefreshScroll False)

                msg ->
                    let
                        _ =
                            Debug.log "unhandled message type" msg
                    in
                        ( model, Cmd.none )

        Nothing ->
            Debug.log "getServer was none?" ( model, Cmd.none )
