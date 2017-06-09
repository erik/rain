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
    = AddServer ServerMetaData
    | AddScrollback ServerInfo ChannelName Line
    | AddLine ServerInfo ChannelName Line
    | ClearChannel ServerInfo ChannelName
    | CloseChannel ServerInfo ChannelName
    | ConnectIrc ServerInfo
    | CreateChannel ServerInfo ChannelName
    | DisconnectServer ServerInfo
    | FormMsg Form.Msg
    | ReceiveRawLine ServerName String
    | ReceiveScrollback ServerName ChannelName Line
    | RefreshScroll Bool
    | SelectChannel ServerInfo ChannelName
    | SendLine ServerInfo ChannelInfo String
    | SendNotification String String
    | SendRawLine ServerInfo String
    | ShowAddServerForm
    | TabCompleteLine ServerInfo ChannelInfo
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
                    , meta = meta
                    , channels = Dict.empty
                    }

                servers_ =
                    model.servers
                        |> Dict.insert meta.name info
            in
                { model | servers = servers_ } ! []

        DisconnectServer serverInfo ->
            { model | servers = Dict.remove serverInfo.name model.servers } ! []

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
            in
                lines
                    |> List.map (SendRawLine server)
                    |> List.foldr (andThen) ( model, Cmd.none )

        AddLine serverInfo channelName line ->
            let
                chanInfo =
                    getOrCreateChannel serverInfo channelName
                        |> \c -> { c | buffer = appendLine c.buffer line }

                model_ =
                    setChannel serverInfo chanInfo model

                nickRegexp =
                    Regex.regex ("\\b" ++ serverInfo.nick ++ "\\b")

                matchesNick =
                    Regex.contains nickRegexp line.message

                isDirectMessage =
                    (serverInfo.nick /= line.nick)
                        && (not (String.startsWith "#" channelName))

                body =
                    String.concat [ "<", line.nick, ">: ", line.message ]

                cmd =
                    if (not chanInfo.isServer) && (matchesNick || isDirectMessage) then
                        SendNotification chanInfo.name body
                    else
                        Noop
            in
                update cmd model_

        AddScrollback serverInfo channelName line ->
            model ! [ Ports.saveScrollback ( serverInfo.name, channelName, line ) ]

        ReceiveScrollback serverName channelName line ->
            case getServer model serverName of
                Just serverInfo ->
                    update (AddLine serverInfo channelName line) model

                _ ->
                    Debug.crash "unknown server" serverName

        SendLine serverInfo chanInfo line ->
            let
                privmsg target msg =
                    let
                        line =
                            { ts = model.currentTime
                            , nick = serverInfo.nick
                            , message = msg
                            }

                        rawLine =
                            String.join " " [ "PRIVMSG", target, ":" ++ msg ]
                    in
                        if chanInfo.isServer then
                            [ AddLine serverInfo serverBufferName line ]
                        else
                            [ SendRawLine serverInfo rawLine
                            , AddLine serverInfo target line
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
                        [ AddLine serverInfo chanInfo.name line ]

                slashCommand cmd params =
                    case ( String.toLower cmd, params ) of
                        ( "/join", [ channel ] ) ->
                            if String.startsWith "#" channel then
                                [ SendRawLine serverInfo ("JOIN " ++ channel)
                                , SelectChannel serverInfo channel
                                ]
                            else
                                addErrorMessage "channel names must begin with #"

                        ( "/query", [ nick ] ) ->
                            if String.startsWith "#" nick then
                                addErrorMessage "can only initiate queries with users"
                            else
                                [ SelectChannel serverInfo nick ]

                        ( "/part", [] ) ->
                            slashCommand "/part" [ chanInfo.name ]

                        ( "/part", [ channel ] ) ->
                            [ SendRawLine serverInfo ("PART " ++ channel)
                            , CloseChannel serverInfo channel
                            ]

                        ( "/close", [] ) ->
                            [ ClearChannel serverInfo chanInfo.name
                            , CloseChannel serverInfo chanInfo.name
                            ]

                        ( "/clear", [] ) ->
                            [ ClearChannel serverInfo chanInfo.name ]

                        ( "/me", rest ) ->
                            let
                                msg =
                                    String.join " " rest
                            in
                                ctcp chanInfo.name "ACTION" msg

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
                                nicks =
                                    chanInfo.users
                                        |> Set.toList
                                        |> List.take 100

                                message =
                                    [ Set.size chanInfo.users |> toString, "users:" ]
                                        ++ nicks

                                line =
                                    { ts = model.currentTime
                                    , message = String.join " " message
                                    , nick = chanInfo.name
                                    }
                            in
                                [ AddLine serverInfo chanInfo.name line ]

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
                            privmsg chanInfo.name line
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

        CreateChannel serverInfo channelName ->
            let
                channel =
                    Model.newChannel channelName

                model_ =
                    setChannel serverInfo channel model
            in
                model_ ! []

        SelectChannel serverInfo channelName ->
            let
                channel =
                    getOrCreateChannel serverInfo channelName
                        |> \chan -> { chan | lastChecked = model.currentTime }

                model_ =
                    setChannel serverInfo channel model
                        |> \model ->
                            { model
                                | current = Just ( serverInfo.name, channelName )
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
                                channelInfo.users
                                    |> Set.filter (String.startsWith w)
                                    |> Set.toList
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
                        Just ( serverInfo, chanInfo ) ->
                            setChannel serverInfo
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

        ClearChannel serverInfo channelName ->
            case getChannel serverInfo channelName of
                Just channel ->
                    let
                        channel_ =
                            { channel | buffer = [] }
                    in
                        (setChannel serverInfo channel_ model)
                            ! [ Ports.clearScrollback ( serverInfo.name, channelName ) ]

                Nothing ->
                    Debug.crash "bad channel name given?" channelName

        CloseChannel serverInfo channelName ->
            let
                current =
                    if model.current == Just ( serverInfo.name, channelName ) then
                        Nothing
                    else
                        model.current

                serverInfo_ =
                    { serverInfo | channels = Dict.remove (String.toLower channelName) serverInfo.channels }

                model_ =
                    { model
                        | current = current
                        , servers = Dict.insert serverInfo.name serverInfo_ model.servers
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
                serverInfo.name
            else
                user.nick

        newLine =
            { ts = ts, nick = nick, message = message }

        newMsg =
            AddLine serverInfo target_ newLine

        refreshMsg =
            if Just ( serverInfo.name, target_ ) == model.current then
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
                channels_ =
                    serverInfo.channels
                        |> Dict.map (\_ v -> { v | buffer = [] })

                serverInfo_ =
                    { serverInfo | channels = channels_ }

                model_ =
                    model.servers
                        |> Dict.insert serverInfo.name serverInfo_
                        |> \info -> { model | servers = info }
            in
                model_ ! [ Ports.requestScrollback serverInfo.name ]

        ( "PING", params ) ->
            let
                pong =
                    ("PONG " ++ (String.concat params))
            in
                update (SendRawLine serverInfo pong) model

        ( "JOIN", [ channel ] ) ->
            let
                chanInfo =
                    getChannel serverInfo channel
                        |> Maybe.withDefault (Model.newChannel channel)

                chanInfo_ =
                    { chanInfo | users = Set.insert msg.user.nick chanInfo.users }

                model_ =
                    setChannel serverInfo chanInfo_ model

                current_ =
                    -- We want to switch to the channel if we haven't
                    -- joined anything else yet.
                    if serverInfo.nick == msg.user.nick && model.current == Nothing then
                        Just ( serverInfo.name, channel )
                    else
                        model.current
            in
                { model_ | current = current_ } ! []

        ( "PART", channel :: reason ) ->
            case getChannel serverInfo channel of
                Just chanInfo ->
                    let
                        chanInfo_ =
                            { chanInfo | users = Set.remove msg.user.nick chanInfo.users }

                        model_ =
                            setChannel serverInfo chanInfo_ model

                        ( current, cmd ) =
                            if serverInfo.nick == msg.user.nick then
                                ( Nothing, CloseChannel serverInfo channel )
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
                    serverInfo.channels
                        |> Dict.map (\_ ch -> { ch | users = Set.remove msg.user.nick ch.users })
                        |> \channels -> { serverInfo | channels = channels }

                model_ =
                    { model | servers = Dict.insert serverInfo.name serverInfo model.servers }
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
                chanInfo =
                    getOrCreateChannel serverInfo target

                chanInfo_ =
                    { chanInfo | topic = Just topic }
            in
                ( setChannel serverInfo chanInfo_ model, Cmd.none )

        ( "333", _ ) ->
            model ! []

        -- NAMES list
        ( "353", [ _, _, channel, usersString ] ) ->
            let
                specialChars =
                    Regex.regex "[%@~\\+]"

                stripSpecial =
                    Regex.replace Regex.All specialChars (\_ -> "")

                chanInfo =
                    getOrCreateChannel serverInfo channel

                userSet =
                    stripSpecial usersString
                        |> String.words
                        |> Set.fromList
                        |> Set.union chanInfo.users

                chanInfo_ =
                    { chanInfo | users = userSet }

                model_ =
                    setChannel serverInfo chanInfo_ model
            in
                model_ ! []

        -- END of /NAMES
        ( "366", _ ) ->
            model ! []

        ( "NICK", [ nick ] ) ->
            let
                server_ =
                    if msg.user.nick == serverInfo.nick then
                        { serverInfo | nick = nick }
                    else
                        serverInfo

                model_ =
                    { model | servers = Dict.insert serverInfo.name server_ model.servers }
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
