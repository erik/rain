module View exposing (view)

import Date
import Date.Format as Date
import Dict
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (id, href, class, title, target, value, classList, placeholder, autofocus)
import Html.Events exposing (onInput, onSubmit, onWithOptions, keyCode, onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Model exposing (..)
import Regex exposing (HowMany(All), regex)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        chatView =
            case ( model.newServerForm, getActive model ) of
                ( Just form, _ ) ->
                    Html.map FormMsg (viewForm form)

                ( Nothing, Just ( server, buffer ) ) ->
                    viewBuffer model server buffer

                _ ->
                    viewHelpText
    in
        div [ id "container" ] [ viewBufferList model, chatView ]


viewHelpText : Html msg
viewHelpText =
    let
        commandDesc =
            [ ( "/server save", "save the configuration for the current server to localstorage" )
            , ( "/server delete", "remove the current configuration from localstorage" )
            , ( "/server disconnect", "close the IRC connection to the current server" )
            , ( "/join #channel", "join and switch to #channel" )
            , ( "/part", "leave the current channel" )
            , ( "/part #channel", "leave #channel" )
            , ( "/close", "close the current buffer window" )
            , ( "/clear", "clear out the contents of the current buffer window" )
            , ( "/ping nick", "send CTCP PING to nick" )
            , ( "/names", "list the (first 100) users in the current channel" )
            , ( "/ns", "shorthand to message NickServ" )
            , ( "/cs", "shorthand to message ChanServ" )
            , ( "/query nick", "open a direct message buffer window with nick" )
            , ( "/quote something", "send \"something\" to the server directly" )
            ]

        commands =
            commandDesc
                |> List.map
                    (\( cmd, desc ) ->
                        li []
                            [ div [ class "command-name" ] [ text cmd ]
                            , div [ class "command-desc" ] [ text desc ]
                            ]
                    )
                |> ul []

        setupSteps =
            [ "start websocket proxy"
            , "configure irc connection through 'add server' dialog"
            , """if everything connects correctly, '/server save' to
                 persist the server config to localStorage"""
            , "☔ "
            ]
                |> List.map (linkifyLine >> (li []))
                |> ol []
    in
        div [ id "buffer-view" ]
            [ h1 [] [ text "rain" ]
            , p [] [ text """Minimal browser based IRC client connecting
                       over a websocket proxy backend.""" ]
            , p [] (linkifyLine "source: https://github.com/erik/rain")
            , h2 [] [ text "01 setup" ]
            , setupSteps
            , h2 [] [ text "02 supported commands" ]
            , commands
            ]


viewForm : Form () ServerMetaData -> Html Form.Msg
viewForm form =
    let
        inputs =
            [ ( Input.textInput, "WebSocket proxy:", "proxyHost", "wss://rain-proxy.example.com/" )
            , ( Input.passwordInput, "WebSocket proxy password:", "proxyPass", "" )
            , ( Input.textInput, "IRC server name:", "name", "freenode" )
            , ( Input.textInput, "IRC server host:", "server", "irc.freenote.net" )
            , ( Input.textInput, "IRC server port:", "port_", "6697" )
            , ( Input.passwordInput, "IRC server password", "pass", "" )
            , ( Input.textInput, "Nick", "nick", "rain`" )
            ]

        inputsHtml =
            inputs
                |> List.map
                    (\( input, lbl, fieldName, ex ) ->
                        div [ class "form-row" ]
                            [ label [] [ text lbl ]
                            , input (Form.getFieldAsString fieldName form)
                                [ placeholder ex ]
                            ]
                    )

        scrollbackInput =
            div [ class "form-row" ]
                [ label [] [ text "Save scrollback" ]
                , Input.checkboxInput (Form.getFieldAsBool "saveScrollback" form) []
                ]
    in
        div [ id "buffer-view" ]
            [ h1 [] [ text "Add IRC Connection" ]
            , div [] (inputsHtml ++ [ scrollbackInput ])
            , div [ class "form-row" ]
                [ button
                    [ onClick Form.Submit ]
                    [ text "Add server" ]
                ]
            ]


hasUnread : BufferInfo -> Bool
hasUnread buf =
    let
        lastMessageTs =
            buf.buffer
                |> List.head
                |> Maybe.andThen (\grp -> List.head grp.messages)
                |> Maybe.map .ts
                |> Maybe.withDefault buf.lastChecked
    in
        buf.lastChecked < lastMessageTs


viewBufferList : Model -> Html Msg
viewBufferList model =
    let
        viewBufInfo serverInfo bufInfo =
            li
                [ onClick (SelectBuffer serverInfo bufInfo.name)
                , classList
                    [ ( "clickable", True )
                    , ( "unread", hasUnread bufInfo )
                    , ( "buffer-list-item", True )
                    ]
                ]
                [ text bufInfo.name ]

        bufferList serverInfo =
            serverInfo.buffers
                |> Dict.values
                |> List.sortBy .name
                |> List.map (lazy2 viewBufInfo serverInfo)

        serverList =
            model.servers
                |> Dict.toList
                |> List.map
                    (\( serverName, serverInfo ) ->
                        div []
                            [ hr [] []
                            , li [ class "clickable" ]
                                [ span [ onClick (SelectBuffer serverInfo serverBufferName) ]
                                    [ text serverName ]
                                , ul [] (bufferList serverInfo)
                                ]
                            ]
                    )

        addServer =
            li [ class "clickable", onClick ShowAddServerForm ] [ text "add server" ]
    in
        div [ id "buffer-list" ] [ ul [] (addServer :: serverList) ]


viewBuffer : Model -> ServerInfo -> BufferInfo -> Html Msg
viewBuffer model server buffer =
    div [ id "buffer-view" ]
        [ div [ id "buffer-header", class "flex-fixed" ]
            [ h1 [] [ text buffer.name ]
            , viewTopic buffer
            ]
        , lazy2 viewBufferMessages server buffer
        , div [ id "buffer-footer", class "flex-fixed" ]
            [ input
                [ id "input-line"
                , placeholder server.nick
                , onInput TypeLine
                , onInputKey model server buffer
                , value model.inputLine
                , autofocus True
                ]
                []
            ]
        ]


enterKey : number
enterKey =
    13


tabKey : number
tabKey =
    9


{-| Handle enter / tab key presses.
Cribbed from elm-todo
-}
onInputKey : Model -> ServerInfo -> BufferInfo -> Attribute Msg
onInputKey model server buffer =
    let
        isKey code =
            if code == enterKey then
                SendLine server buffer model.inputLine
                    |> Json.succeed
            else if code == tabKey then
                TabCompleteLine server buffer
                    |> Json.succeed
            else
                Json.fail "nope"

        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "keydown" options (Json.andThen isKey keyCode)


viewTopic : BufferInfo -> Html Msg
viewTopic buffer =
    let
        topic =
            Maybe.withDefault "" buffer.topic
    in
        div [ id "buffer-topic" ] (linkifyLine topic)


viewBufferMessages : ServerInfo -> BufferInfo -> Html Msg
viewBufferMessages serverInfo buffer =
    let
        lines =
            buffer.buffer
                |> List.map (viewLineGroup serverInfo)
    in
        div [ id "buffer-messages" ] lines


viewLineGroup : ServerInfo -> LineGroup -> Html Msg
viewLineGroup serverInfo group =
    let
        timeStr =
            group.ts
                |> Date.fromTime
                |> Date.format "%H:%M:%S"

        groupHead =
            div [ class "group-head" ]
                [ small [ class "timestamp" ] [ text timeStr ]
                , span [] [ text " " ]
                , div
                    [ classList
                        [ ( "message-nick", True )
                        , ( "message-nick-self", group.nick == serverInfo.nick )
                        ]
                    ]
                    [ span
                        [ class "clickable"
                        , onClick (SelectBuffer serverInfo group.nick)
                        ]
                        [ text group.nick ]
                    ]
                ]

        messages =
            List.map (formatLine serverInfo) group.messages
    in
        div [ class "group" ]
            [ groupHead
            , div [ class "group-messages" ] messages
            ]


linkifyLine : String -> List (Html msg)
linkifyLine line =
    let
        words =
            String.split " " line

        linkify word =
            if String.contains "://" word then
                a [ href word, target "_blank" ] [ text word ]
            else
                text word
    in
        words
            |> List.map linkify
            |> List.intersperse (span [] [ text " " ])


formatLine : ServerInfo -> Line -> Html Msg
formatLine serverInfo line =
    let
        timeStr =
            line.ts
                |> Date.fromTime
                |> Date.format "%Y-%m-%d %H:%M:%S"

        ( message, isAction ) =
            case String.split ("\x01" ++ "ACTION") line.message of
                "" :: rest ->
                    let
                        message =
                            rest
                                |> String.concat
                                |> String.dropRight 1
                    in
                        ( String.join " " [ line.nick, message ], True )

                _ ->
                    ( line.message, False )

        matchesNick =
            line.message
                |> Regex.contains (regex ("\\b" ++ serverInfo.nick ++ "\\b"))

        copyText =
            span [ class "copy-text" ] [ text ("<" ++ line.nick ++ "> ") ]

        linkified =
            linkifyLine message
    in
        div
            [ title timeStr
            , classList
                [ ( "highlight", matchesNick )
                , ( "action", isAction )
                ]
            ]
            (copyText :: linkified)
