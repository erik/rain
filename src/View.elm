module View exposing (view)

import Date
import Date.Format as Date
import Dict
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (id, href, class, title, target, value, classList, placeholder, autofocus)
import Html.Events exposing (onInput, onSubmit, onWithOptions, keyCode, onClick)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Model exposing (..)
import Regex exposing (HowMany(All, AtMost), regex)
import Update exposing (Msg(..), ServerMsg(..))


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
        commands =
            Update.commandDescriptions
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


viewForm : Form () ServerMetadata -> Html Form.Msg
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


hasUnread : Buffer -> Bool
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
        isCurrent serverName bufferName =
            model.current == Just ( serverName, bufferName )

        viewBuf serverName buf =
            li
                [ onClick (ModifyServer serverName (SelectBuffer buf.name))
                , classList
                    [ ( "clickable", True )
                    , ( "unread", hasUnread buf )
                    , ( "buffer-list-item", True )
                    , ( "current-buffer", isCurrent serverName buf.name )
                    ]
                ]
                [ text buf.name ]

        bufferList server =
            server.buffers
                |> Dict.values
                |> List.filter (\buf -> not buf.isServer)
                |> List.sortBy .name
                |> List.map (viewBuf server.meta.name)

        serverList =
            model.servers
                |> Dict.toList
                |> List.map
                    (\( serverName, server ) ->
                        div []
                            [ hr [] []
                            , li [ class "clickable" ]
                                [ span
                                    [ onClick
                                        (ModifyServer serverName
                                            (SelectBuffer serverBufferName)
                                        )
                                    ]
                                    [ text serverName ]
                                , ul [] (bufferList server)
                                ]
                            ]
                    )

        addServer =
            li [ class "clickable", onClick ShowAddServerForm ] [ text "add server" ]
    in
        div [ id "buffer-list" ] [ ul [] (addServer :: serverList) ]


viewBuffer : Model -> Server -> Buffer -> Html Msg
viewBuffer model server buffer =
    let
        bufferName =
            if buffer.isServer then
                server.meta.name
            else
                buffer.name
    in
        div [ id "buffer-view" ]
            [ div [ id "buffer-header", class "flex-fixed" ]
                [ h1 [] [ text bufferName ]
                , viewTopic buffer
                ]
            , lazy2 viewBufferMessages server.meta buffer.buffer
            , div [ id "buffer-footer", class "flex-fixed" ]
                [ input
                    [ id "input-line"
                    , placeholder server.meta.nick
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
onInputKey : Model -> Server -> Buffer -> Attribute Msg
onInputKey model server buffer =
    let
        isKey code =
            if code == enterKey then
                SendLine buffer model.inputLine
                    |> ModifyServer server.meta.name
                    |> Json.succeed
            else if code == tabKey then
                TabCompleteLine buffer
                    |> ModifyServer server.meta.name
                    |> Json.succeed
            else
                Json.fail "nope"

        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "keydown" options (Json.andThen isKey keyCode)


viewTopic : Buffer -> Html Msg
viewTopic buffer =
    let
        topic =
            Maybe.withDefault "" buffer.topic
    in
        div [ id "buffer-topic" ] (linkifyLine topic)


viewBufferMessages : ServerMetadata -> LineBuffer -> Html Msg
viewBufferMessages serverMeta buffer =
    buffer
        |> List.map (viewLineGroup serverMeta)
        |> div [ id "buffer-messages" ]


viewLineGroup : ServerMetadata -> LineGroup -> Html Msg
viewLineGroup serverMeta group =
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
                        , ( "message-nick-self", group.nick == serverMeta.nick )
                        ]
                    ]
                    [ span
                        [ class "clickable"
                        , onClick (ModifyServer serverMeta.name (SelectBuffer group.nick))
                        ]
                        [ text group.nick ]
                    ]
                ]

        formatMessages msgs =
            List.map (formatLine serverMeta.nick) msgs
                |> div [ class "group-messages" ]
    in
        div [ class "group" ]
            [ groupHead
            , lazy formatMessages group.messages
            ]


linkifyLine : String -> List (Html msg)
linkifyLine line =
    let
        -- Simple approximation of one at least.
        linkRegex =
            regex "\\b(\\w+://[-\\w@:%._\\+~#=/\\?]+)\\b"

        linkify word url =
            a [ href url, target "_blank" ] [ text word ]

        applyMarkup word =
            case Regex.find (AtMost 1) linkRegex word of
                [] ->
                    text word

                url :: [] ->
                    linkify word url.match

                _ ->
                    Debug.crash ("Linkify failed on" ++ word)
    in
        line
            |> String.split " "
            |> List.map applyMarkup
            |> List.intersperse (span [] [ text " " ])


formatLine : String -> Line -> Html Msg
formatLine nick line =
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
                |> Regex.contains (regex ("\\b" ++ nick ++ "\\b"))

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
