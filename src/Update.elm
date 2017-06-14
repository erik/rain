module Update exposing (Msg(..), update)

import Debug
import Dict
import Form exposing (Form)
import Irc
import Model exposing (..)
import Ports
import Regex
import Set exposing (Set)
import Time exposing (Time)
import WebSocket


type StoreServerAction
    = StoreServer
    | RemoveServer


type Msg
    = AddServer ServerMetadata
    | AddScrollback ServerInfo BufferName Line
    | AddLine ServerInfo BufferName Line
    | ClearBuffer ServerInfo BufferName
    | CloseBuffer ServerInfo BufferName
    | ConnectIrc ServerInfo
    | CreateBuffer ServerInfo BufferName
    | DisconnectServer ServerInfo
    | FormMsg Form.Msg
    | ReceiveRawLine ServerName String
    | ReceiveScrollback ServerName BufferName Line
    | RefreshScroll Bool
    | SelectBuffer ServerName BufferName
    | SendLine ServerInfo BufferInfo String
    | SendNotification String String
    | SendRawLine ServerInfo String
    | ShowAddServerForm
    | TabCompleteLine ServerInfo BufferInfo
    | Tick Time
    | TypeLine String
    | UpdateServerStore ServerInfo StoreServerAction
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
                    String.concat [ meta.proxyHost, "?", queryString ]

                pass =
                    -- TODO: meta.pass should be a maybe in the first place.
                    if meta.pass == "" then
                        Nothing
                    else
                        Just meta.pass

                info =
                    { socket = socketUrl
                    , pass = pass
                    , meta = meta
                    , buffers = Dict.empty
                    }

                servers_ =
                    model.servers
                        |> Dict.insert meta.name info
            in
                { model | servers = servers_ } ! []

        DisconnectServer serverInfo ->
            { model | servers = Dict.remove serverInfo.meta.name model.servers } ! []

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
                    , "NICK " ++ server.meta.nick
                    , "USER " ++ server.meta.nick ++ " * * :" ++ server.meta.nick
                    ]
            in
                lines
                    |> List.map (SendRawLine server)
                    |> List.foldr (andThen) ( model, Cmd.none )

        AddLine serverInfo bufferName line ->
            let
                bufInfo =
                    getOrCreateBuffer serverInfo bufferName
                        |> \b -> { b | buffer = appendLine b.buffer line }

                model_ =
                    setBuffer serverInfo bufInfo model

                nickRegexp =
                    Regex.regex ("\\b" ++ serverInfo.meta.nick ++ "\\b")

                matchesNick =
                    Regex.contains nickRegexp line.message

                isDirectMessage =
                    (serverInfo.meta.nick /= line.nick)
                        && (not (String.startsWith "#" bufferName))

                body =
                    String.concat [ "<", line.nick, ">: ", line.message ]

                cmd =
                    if (not bufInfo.isServer) && (matchesNick || isDirectMessage) then
                        SendNotification bufInfo.name body
                    else
                        Noop
            in
                update cmd model_

        AddScrollback serverInfo bufferName line ->
            model ! [ Ports.saveScrollback ( serverInfo.meta.name, bufferName, line ) ]

        ReceiveScrollback serverName bufferName line ->
            case getServer model serverName of
                Just serverInfo ->
                    update (AddLine serverInfo bufferName line) model

                _ ->
                    Debug.crash "unknown server" serverName

        SendLine serverInfo bufInfo line ->
            let
                privmsg target msg =
                    let
                        line =
                            { ts = model.currentTime
                            , nick = serverInfo.meta.nick
                            , message = msg
                            }

                        rawLine =
                            String.join " " [ "PRIVMSG", target, ":" ++ msg ]
                    in
                        if bufInfo.isServer then
                            addErrorMessage "use /quote to send messages directly to the server"
                        else
                            [ SendRawLine serverInfo rawLine
                            , AddLine serverInfo target line
                            , if serverInfo.meta.saveScrollback then
                                AddScrollback serverInfo target line
                              else
                                Noop
                            ]

                ctcp target command msg =
                    let
                        msg_ =
                            String.concat [ "\x01", command, " ", msg, "\x01" ]
                    in
                        privmsg target msg_

                -- shortened versions of common commands
                commandAlias cmd =
                    Dict.fromList
                        [ ( "/j", "/join" )
                        , ( "/msg", "/privmsg" )
                        , ( "/pm", "/privmsg" )
                        , ( "/q", "/query" )
                        ]
                        |> Dict.get cmd
                        |> Maybe.withDefault cmd

                addErrorMessage msg =
                    let
                        line =
                            { ts = model.currentTime
                            , nick = "*error"
                            , message = msg
                            }
                    in
                        [ AddLine serverInfo bufInfo.name line ]

                slashCommand cmd params =
                    case ( String.toLower cmd, params ) of
                        ( "/join", [ channel ] ) ->
                            if String.startsWith "#" channel then
                                [ SendRawLine serverInfo ("JOIN " ++ channel)
                                , SelectBuffer serverInfo.meta.name channel
                                ]
                            else
                                addErrorMessage "channel names must begin with #"

                        ( "/query", [ nick ] ) ->
                            if String.startsWith "#" nick then
                                addErrorMessage "can only initiate queries with users"
                            else
                                [ SelectBuffer serverInfo.meta.name nick ]

                        ( "/part", [] ) ->
                            slashCommand "/part" [ bufInfo.name ]

                        ( "/part", [ channel ] ) ->
                            [ SendRawLine serverInfo ("PART " ++ channel)
                            , CloseBuffer serverInfo channel
                            ]

                        ( "/close", [] ) ->
                            [ ClearBuffer serverInfo bufInfo.name
                            , CloseBuffer serverInfo bufInfo.name
                            ]

                        ( "/clear", [] ) ->
                            [ ClearBuffer serverInfo bufInfo.name ]

                        ( "/me", rest ) ->
                            let
                                msg =
                                    String.join " " rest
                            in
                                ctcp bufInfo.name "ACTION" msg

                        ( "/privmsg", target :: rest ) ->
                            privmsg target (String.join " " rest)

                        ( "/ping", [ target ] ) ->
                            ctcp target "PING" (toString model.currentTime)

                        ( "/ns", rest ) ->
                            privmsg "NickServ" (String.join " " rest)

                        ( "/cs", rest ) ->
                            privmsg "ChanServ" (String.join " " rest)

                        ( "/names", [] ) ->
                            let
                                nickList =
                                    case bufInfo.users of
                                        UsersLoading list ->
                                            list

                                        UsersLoaded set ->
                                            Set.toList set

                                nicks =
                                    List.take 100 nickList

                                message =
                                    [ List.length nickList |> toString, "users:" ]
                                        ++ nicks

                                line =
                                    { ts = model.currentTime
                                    , message = String.join " " message
                                    , nick = bufInfo.name
                                    }
                            in
                                [ AddLine serverInfo bufInfo.name line ]

                        ( "/server", [ "save" ] ) ->
                            [ UpdateServerStore serverInfo StoreServer ]

                        ( "/server", [ "delete" ] ) ->
                            [ UpdateServerStore serverInfo RemoveServer ]

                        ( "/server", [ "disconnect" ] ) ->
                            [ DisconnectServer serverInfo ]

                        ( "/quote", rest ) ->
                            [ SendRawLine serverInfo (String.join " " rest) ]

                        _ ->
                            addErrorMessage "unknown command, did you forget to /quote?"

                messages =
                    case ( String.left 1 line, String.words line ) of
                        ( "/", cmd :: params ) ->
                            slashCommand (commandAlias cmd) params

                        _ ->
                            privmsg bufInfo.name line
            in
                if model.inputLine == "" then
                    model ! []
                else
                    { model | inputLine = "" }
                        |> batchMessage messages
                        |> andThen (RefreshScroll True)

        TypeLine str ->
            { model | inputLine = str } ! []

        SendRawLine serverInfo line ->
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
                                |> Maybe.withDefault model.currentTime
                    in
                        if line == "AUTHENTICATE" then
                            update (ConnectIrc serverInfo)
                        else
                            msg
                                |> Maybe.map (handleCommand serverInfo ts)
                                |> Maybe.withDefault (\model -> ( model, Cmd.none ))
            in
                case getServer model serverName of
                    Just serverInfo ->
                        String.trim line
                            |> String.lines
                            |> List.filter (not << String.isEmpty)
                            |> List.map (handleLine serverInfo)
                            |> List.foldl
                                (\handler ( model, cmd ) ->
                                    let
                                        ( model_, cmd_ ) =
                                            handler model
                                    in
                                        model_ ! [ cmd, cmd_ ]
                                )
                                ( model, Cmd.none )

                    _ ->
                        Debug.crash "tried to select a bad server"

        CreateBuffer serverInfo bufferName ->
            let
                buffer =
                    Model.newBuffer bufferName

                model_ =
                    setBuffer serverInfo buffer model
            in
                model_ ! []

        SelectBuffer serverName bufferName ->
            case getServer model serverName of
                Just serverInfo ->
                    let
                        buffer =
                            getOrCreateBuffer serverInfo bufferName
                                |> \chan -> { chan | lastChecked = model.currentTime }

                        model_ =
                            setBuffer serverInfo buffer model
                                |> \model ->
                                    { model
                                        | current = Just ( serverInfo.meta.name, bufferName )
                                        , newServerForm = Nothing
                                    }
                    in
                        update (RefreshScroll True) model_

                Nothing ->
                    Debug.crash "tried to select bad server"

        RefreshScroll force ->
            ( model, Ports.refreshScrollPosition force )

        SendNotification title message ->
            ( model, Ports.sendNotification ( title, message ) )

        TabCompleteLine serverInfo bufferInfo ->
            let
                words =
                    String.split " " model.inputLine

                lastWord =
                    List.reverse words
                        |> List.filter (not << String.isEmpty)
                        |> List.head

                -- TODO: should also complete /privmsg etc
                completions =
                    lastWord
                        |> Maybe.map
                            (\w ->
                                (case bufferInfo.users of
                                    UsersLoading list ->
                                        list

                                    UsersLoaded set ->
                                        Set.toList set
                                )
                                    |> List.filter (String.startsWith w)
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
                        model ! []

        Tick time ->
            let
                model_ =
                    case getActive model of
                        Just ( serverInfo, bufInfo ) ->
                            setBuffer serverInfo
                                { bufInfo | lastChecked = time }
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
                        { model
                            | newServerForm =
                                form_
                                    |> Maybe.map (Form.update newServerValidation formMsg)
                        }
                            ! []

        ShowAddServerForm ->
            let
                form =
                    Form.initial [] newServerValidation
            in
                { model | newServerForm = Just form } ! []

        ClearBuffer serverInfo bufferName ->
            case getBuffer serverInfo bufferName of
                Just buffer ->
                    let
                        buffer_ =
                            { buffer | buffer = [] }
                    in
                        (setBuffer serverInfo buffer_ model)
                            ! [ Ports.clearScrollback ( serverInfo.meta.name, bufferName ) ]

                Nothing ->
                    Debug.crash "bad buffer name given?" bufferName

        CloseBuffer serverInfo bufferName ->
            let
                current =
                    if model.current == Just ( serverInfo.meta.name, bufferName ) then
                        Nothing
                    else
                        model.current

                serverInfo_ =
                    { serverInfo | buffers = Dict.remove (String.toLower bufferName) serverInfo.buffers }

                model_ =
                    { model
                        | current = current
                        , servers = Dict.insert serverInfo.meta.name serverInfo_ model.servers
                    }
            in
                model_ ! []

        UpdateServerStore serverInfo action ->
            let
                actionStr =
                    case action of
                        StoreServer ->
                            "STORE"

                        RemoveServer ->
                            "REMOVE"
            in
                ( model, Ports.modifyServerStore ( serverInfo.meta, actionStr ) )

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


batchMessage : List Msg -> Model -> ( Model, Cmd Msg )
batchMessage msgs model =
    List.foldl (andThen) ( model, Cmd.none ) msgs


handleMessage : ServerInfo -> UserInfo -> String -> String -> Time.Time -> Model -> ( Model, Cmd Msg )
handleMessage serverInfo user target message ts model =
    let
        target_ =
            if String.startsWith "#" target then
                target
            else
                user.nick

        nick =
            if user.isServer then
                serverInfo.meta.name
            else
                user.nick

        newLine =
            { ts = ts, nick = nick, message = message }

        newMsg =
            AddLine serverInfo target_ newLine

        refreshMsg =
            if Just ( serverInfo.meta.name, target_ ) == model.current then
                RefreshScroll False
            else
                Noop

        scrollbackMsg =
            if (not user.isServer) && serverInfo.meta.saveScrollback then
                AddScrollback serverInfo target_ newLine
            else
                Noop
    in
        update newMsg model
            |> andThen refreshMsg
            |> andThen scrollbackMsg


handleCommand : ServerInfo -> Time.Time -> Irc.ParsedMessage -> Model -> ( Model, Cmd Msg )
handleCommand serverInfo ts msg model =
    case ( msg.command, msg.params ) of
        -- Clean out the buffers when we rejoin.
        ( "001", _ ) ->
            let
                buffers_ =
                    serverInfo.buffers
                        |> Dict.map (\_ v -> { v | buffer = [] })

                serverInfo_ =
                    { serverInfo | buffers = buffers_ }

                model_ =
                    model.servers
                        |> Dict.insert serverInfo.meta.name serverInfo_
                        |> \info -> { model | servers = info }
            in
                model_ ! [ Ports.requestScrollback serverInfo.meta.name ]

        ( "PING", params ) ->
            let
                pong =
                    ("PONG " ++ (String.concat params))
            in
                update (SendRawLine serverInfo pong) model

        ( "JOIN", [ channel ] ) ->
            let
                weJoined =
                    serverInfo.meta.nick == msg.user.nick

                buffer =
                    getBuffer serverInfo channel
                        |> Maybe.withDefault (newBuffer channel)
                        |> addNicks [ msg.user.nick ]

                -- don't report unread messages in the past.
                lastChecked =
                    if weJoined then
                        model.currentTime
                    else
                        buffer.lastChecked

                model_ =
                    setBuffer serverInfo { buffer | lastChecked = lastChecked } model

                current_ =
                    -- We want to switch to the channel if we haven't
                    -- joined anything else yet.
                    if weJoined && model.current == Nothing then
                        Just ( serverInfo.meta.name, channel )
                    else
                        model.current
            in
                { model_ | current = current_ } ! []

        ( "PART", channel :: reason ) ->
            case getBuffer serverInfo channel of
                Just bufInfo ->
                    let
                        bufInfo_ =
                            removeNick msg.user.nick bufInfo

                        model_ =
                            setBuffer serverInfo bufInfo_ model

                        ( current, cmd ) =
                            if serverInfo.meta.nick == msg.user.nick then
                                ( Nothing, CloseBuffer serverInfo channel )
                            else
                                ( model.current, Noop )
                    in
                        update cmd { model_ | current = current }

                Nothing ->
                    let
                        _ =
                            Debug.log "odd: PART for channel we aren't in." channel
                    in
                        ( model, Cmd.none )

        ( "QUIT", _ ) ->
            let
                serverInfo_ =
                    serverInfo.buffers
                        |> Dict.map (\_ buf -> removeNick msg.user.nick buf)
                        |> \buffers -> { serverInfo | buffers = buffers }

                model_ =
                    { model | servers = Dict.insert serverInfo.meta.name serverInfo model.servers }
            in
                ( model_, Cmd.none )

        ( "PRIVMSG", [ target, message ] ) ->
            handleMessage serverInfo msg.user target message ts model

        ( "NOTICE", [ target, message ] ) ->
            let
                formatCtcp msg =
                    case String.words msg of
                        [ "PING", timeString ] ->
                            let
                                time =
                                    timeString
                                        |> String.toFloat
                                        |> Result.withDefault 0

                                pingTime =
                                    (model.currentTime - time)
                                        |> Time.inSeconds
                                        |> toString
                            in
                                "PONG: " ++ pingTime ++ " secs"

                        _ ->
                            "CTCP:" ++ msg

                notice =
                    if String.startsWith "\x01" message then
                        formatCtcp (String.split "\x01" message |> String.concat)
                    else
                        "NOTICE: " ++ message
            in
                handleMessage serverInfo msg.user target notice ts model

        -- You have been marked as being away
        ( "306", _ ) ->
            model ! []

        -- Channel topic
        ( "332", [ _, target, topic ] ) ->
            let
                bufInfo =
                    getOrCreateBuffer serverInfo target

                bufInfo_ =
                    { bufInfo | topic = Just topic }
            in
                ( setBuffer serverInfo bufInfo_ model, Cmd.none )

        ( "333", _ ) ->
            model ! []

        -- NAMES list
        ( "353", [ _, _, channel, usersString ] ) ->
            let
                specialChars =
                    Regex.regex "[%@~\\+]"

                stripSpecial =
                    Regex.replace Regex.All specialChars (\_ -> "")

                userList =
                    stripSpecial usersString
                        |> String.words

                bufInfo =
                    getOrCreateBuffer serverInfo channel
                        |> addNicks userList

                model_ =
                    setBuffer serverInfo bufInfo model
            in
                model_ ! []

        -- END of /NAMES
        ( "366", [ _, channel, _ ] ) ->
            case getBuffer serverInfo channel of
                Just bufInfo ->
                    let
                        users =
                            case bufInfo.users of
                                UsersLoading list ->
                                    UsersLoaded (Set.fromList list)

                                set ->
                                    set

                        model_ =
                            setBuffer serverInfo { bufInfo | users = users } model
                    in
                        model_ ! []

                _ ->
                    let
                        _ =
                            Debug.log "weird: 366 for unknown channel" channel
                    in
                        model ! []

        ( "NICK", [ nick ] ) ->
            let
                myNick =
                    if msg.user.nick == serverInfo.meta.nick then
                        nick
                    else
                        serverInfo.meta.nick

                server =
                    serverInfo.meta
                        |> \m ->
                            { m | nick = myNick }
                                |> \m -> { serverInfo | meta = m }

                model_ =
                    { model | servers = Dict.insert serverInfo.meta.name server model.servers }
            in
                model_ ! []

        _ ->
            let
                msgText =
                    String.join " " msg.params

                newLine =
                    { ts = ts
                    , nick = msg.user.nick
                    , message = String.join ": " [ msg.command, msgText ]
                    }

                _ =
                    Debug.log "unknown msg" msg
            in
                model
                    |> batchMessage
                        [ AddLine serverInfo serverBufferName newLine
                        , RefreshScroll False
                        ]
