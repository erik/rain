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

                ( Nothing, Just serverChan ) ->
                    viewChannel model serverChan

                _ ->
                    div [] [ text "nothing." ]
    in
        div []
            [ lazy viewChannelList model
            , chatView
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
                    (\( input, label, fieldName, example ) ->
                        ( input, label, Form.getFieldAsString fieldName form, example )
                    )
                |> List.map
                    (\( input, lbl, field, ex ) ->
                        div [ class "form-row" ]
                            [ label [] [ text lbl ]
                            , input field [ placeholder ex ]
                            ]
                    )
    in
        div [ id "new-server-form" ]
            [ h1 [] [ text "Add IRC Connection" ]
            , div [] inputsHtml
            , div [ class "form-row" ]
                [ button
                    [ onClick Form.Submit ]
                    [ text "Add server" ]
                ]
            ]


hasUnread : ChannelInfo -> Bool
hasUnread chan =
    let
        lastMsg =
            chan.buffer
                |> List.head
                |> Maybe.map .messages
                |> Maybe.map List.reverse
                |> Maybe.andThen List.head

        lastMsgTs =
            case lastMsg of
                Just msg ->
                    Date.toTime msg.ts

                Nothing ->
                    chan.lastChecked
    in
        chan.lastChecked < lastMsgTs


viewChannelList : Model -> Html Msg
viewChannelList model =
    let
        channelList serverName channels =
            channels
                |> Dict.values
                |> List.sortBy .name
                |> List.map
                    (\chanInfo ->
                        li
                            [ onClick (SelectChannel serverName chanInfo.name)
                            , classList
                                [ ( "clickable", True )
                                , ( "unread", hasUnread chanInfo )
                                ]
                            ]
                            [ text chanInfo.name ]
                    )

        serverList =
            model.serverInfo
                |> Dict.toList
                |> List.map
                    (\( serverName, serverInfo ) ->
                        li [ class "clickable" ]
                            [ span [ onClick (SelectChannel serverName serverBufferName) ]
                                [ text serverName ]
                            , ul [] (channelList serverName serverInfo.channels)
                            ]
                    )

        addServer =
            li [ class "clickable", onClick ShowAddServerForm ] [ text "add server" ]
    in
        aside [ id "channel-list" ] [ ul [] (addServer :: serverList) ]


viewChannel : Model -> ( ServerInfo, ChannelInfo ) -> Html Msg
viewChannel model ( server, channel ) =
    div [ id "channel-view" ]
        [ div [ id "channel-header" ]
            [ h3 [] [ text channel.name ]
            , viewTopic channel
            , hr [] []
            ]
        , lazy2 viewBuffer server channel
        , div [ id "channel-footer" ]
            [ input
                [ id "input-line"
                , placeholder server.nick
                , onInput TypeLine
                , onInputKey model server channel
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
onInputKey : Model -> ServerInfo -> ChannelInfo -> Attribute Msg
onInputKey model server channel =
    let
        isKey code =
            if code == enterKey then
                SendLine server channel model.inputLine
                    |> Json.succeed
            else if code == tabKey then
                TabCompleteLine server channel
                    |> Json.succeed
            else
                Json.fail "nope"

        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "keydown" options (Json.andThen isKey keyCode)


viewTopic : ChannelInfo -> Html Msg
viewTopic channel =
    div [ id "topic" ] [ text <| Maybe.withDefault "" channel.topic ]


viewBuffer : ServerInfo -> ChannelInfo -> Html Msg
viewBuffer serverInfo channel =
    let
        lines =
            channel.buffer
                |> List.reverse
                |> List.map (viewLineGroup serverInfo)
    in
        div [ id "buffer-view" ] lines


viewLineGroup : ServerInfo -> LineGroup -> Html Msg
viewLineGroup serverInfo group =
    let
        timeStr =
            Date.format "%H:%M:%S" group.ts

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
                        , onClick (SelectChannel serverInfo.name group.nick)
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


formatLine : ServerInfo -> Line -> Html Msg
formatLine serverInfo line =
    let
        timeStr =
            Date.format "%Y-%m-%d %H:%M:%S" line.ts

        ( message, isAction ) =
            case String.split ("\x01" ++ "ACTION") line.message of
                "" :: rest ->
                    let
                        message =
                            rest
                                |> String.join ""
                                |> String.dropRight 1
                    in
                        ( String.join " " [ line.nick, message ], True )

                _ ->
                    ( line.message, False )

        split =
            Regex.split All (regex "(\\s+)") message

        matchesNick =
            line.message
                |> Regex.contains (regex ("\\b" ++ serverInfo.nick ++ "\\b"))

        linkify word =
            if String.contains "://" word then
                a [ href word, target "_blank" ] [ text word ]
            else
                text word
    in
        div
            [ title timeStr
            , classList
                [ ( "highlight", matchesNick )
                , ( "action", isAction )
                ]
            ]
            (List.map linkify split)
