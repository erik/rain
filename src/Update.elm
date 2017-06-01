module Update exposing (Msg(..), update)

import Date exposing (Date)
import Debug
import Dict
import Form exposing (Form)
import Irc
import Model exposing (..)
import Ports
import Regex
import Time exposing (Time)
import WebSocket


type Msg
    = AddServer ServerMetaData
    | AddLine ServerName ChannelName Line
    | CloseChannel ServerName ChannelName
    | ConnectIrc ServerInfo
    | CreateChannel ServerName ChannelName
    | FormMsg Form.Msg
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

        ConnectIrc server ->
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
                        if chanInfo.isServer then
                            ( msg, AddLine serverInfo.name serverBufferName line )
                        else
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
            let
                handleLine serverInfo line =
                    let
                        msg =
                            Irc.splitMessage line

                        -- Grab the time out of the message or default to current time.
                        ts =
                            msg
                                |> Maybe.andThen .time
                                |> Maybe.withDefault (Date.fromTime model.currentTime)
                    in
                        if line == "AUTHENTICATE" then
                            update (ConnectIrc serverInfo)
                        else
                            Maybe.map (\msg -> handleCommand serverInfo msg ts) msg
                                |> Maybe.withDefault (\model -> ( model, Cmd.none ))
            in
                case getServer model serverName of
                    Just serverInfo ->
                        String.trim line
                            |> String.lines
                            |> List.filter (not << String.isEmpty)
                            |> List.map (handleLine serverInfo)
                            |> List.foldr
                                (\handler ( model, cmd ) ->
                                    let
                                        ( model_, cmd_ ) =
                                            handler model
                                    in
                                        model_ ! [ cmd, cmd_ ]
                                )
                                ( model, Cmd.none )

                    _ ->
                        Debug.crash "getServer was none?" ( model, Cmd.none )

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


handleMessage : ServerInfo -> UserInfo -> String -> String -> Date -> Model -> ( Model, Cmd Msg )
handleMessage serverInfo user target message ts model =
    let
        target_ =
            if String.startsWith "#" target then
                target
            else
                user.nick

        nick =
            if user.isServer then
                serverInfo.name
            else
                user.nick

        newLine =
            { ts = ts, nick = nick, message = message }

        newMsg =
            AddLine serverInfo.name target_ newLine

        refreshMsg =
            if Just ( serverInfo.name, target_ ) == model.current then
                RefreshScroll False
            else
                Noop
    in
        update newMsg model
            |> andThen refreshMsg


handleCommand : ServerInfo -> Irc.ParsedMessage -> Date -> Model -> ( Model, Cmd Msg )
handleCommand serverInfo msg date model =
    case ( msg.command, msg.params ) of
        -- Clean out the buffers when we rejoin.
        -- TODO: this should be conditional on whether or not this is a bouncer.
        ( "001", _ ) ->
            let
                channels_ =
                    serverInfo.channels
                        |> Dict.map (\_ v -> { v | buffer = [] })

                serverInfo_ =
                    { serverInfo | channels = channels_ }

                model_ =
                    model.serverInfo
                        |> Dict.insert serverInfo.name serverInfo_
                        |> \x -> { model | serverInfo = x }
            in
                ( model_, Cmd.none )

        ( "PING", params ) ->
            update (SendRawLine serverInfo ("PONG " ++ (String.concat params))) model

        ( "JOIN", [ channel ] ) ->
            let
                serverChan =
                    ( serverInfo.name, channel )

                chanInfo =
                    getChannel model serverChan
                        |> Maybe.withDefault (Model.newChannel channel)

                chanInfo_ =
                    { chanInfo | users = Dict.insert msg.user.nick msg.user chanInfo.users }

                model_ =
                    setChannel serverChan chanInfo_ model

                current_ =
                    if serverInfo.nick == msg.user.nick then
                        Just serverChan
                    else
                        model.current
            in
                ( { model_ | current = current_ }, Cmd.none )

        ( "PRIVMSG", [ target, message ] ) ->
            handleMessage serverInfo msg.user target message date model

        ( "NOTICE", [ target, message ] ) ->
            let
                notice =
                    "NOTICE: " ++ message
            in
                handleMessage serverInfo msg.user target notice date model

        -- You have been marked as being away
        ( "306", _ ) ->
            ( model, Cmd.none )

        -- Channel topic
        ( "332", [ _, target, topic ] ) ->
            let
                chanInfo =
                    getOrCreateChannel model ( serverInfo.name, target )

                chanInfo_ =
                    { chanInfo | topic = Just topic }
            in
                ( setChannel ( serverInfo.name, target ) chanInfo_ model, Cmd.none )

        ( "333", _ ) ->
            ( model, Cmd.none )

        -- NAMES list
        ( "353", [ _, _, channel, usersString ] ) ->
            let
                specialChars =
                    Regex.regex "[%@~\\+]+"

                mkUserInfo nickStr =
                    nickStr
                        |> Regex.replace Regex.All specialChars (\_ -> "")
                        |> \nick -> { isServer = False, nick = nick, host = "", real = "" }

                chanInfo =
                    getOrCreateChannel model ( serverInfo.name, channel )

                userDict =
                    String.words usersString
                        |> List.map mkUserInfo
                        |> List.map (\u -> ( u.nick, u ))
                        |> Dict.fromList
                        |> Dict.union chanInfo.users

                chanInfo_ =
                    { chanInfo | users = userDict }

                model_ =
                    setChannel ( serverInfo.name, channel ) chanInfo_ model
            in
                ( model_, Cmd.none )

        -- END of /NAMES
        ( "366", _ ) ->
            ( model, Cmd.none )

        ( "NICK", [ nick ] ) ->
            let
                server_ =
                    if msg.user.nick == serverInfo.nick then
                        { serverInfo | nick = nick }
                    else
                        serverInfo

                model_ =
                    { model | serverInfo = Dict.insert serverInfo.name server_ model.serverInfo }
            in
                ( model_, Cmd.none )

        _ ->
            let
                msgText =
                    msg.params
                        |> String.join " "

                newLine =
                    { ts = date
                    , nick = msg.user.nick
                    , message = String.join ": " [ msg.command, msgText ]
                    }

                newMsg =
                    AddLine serverInfo.name serverBufferName newLine

                _ =
                    Debug.log "unknown msg" msg
            in
                update newMsg model
                    |> andThen (RefreshScroll False)
