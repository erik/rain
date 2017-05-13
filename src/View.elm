module View exposing (view)

import Date.Format as Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (id, href, class, title, target, value, classList, placeholder, autofocus)
import Html.Events exposing (onInput, onSubmit, onWithOptions, keyCode, onClick)
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Model exposing (..)
import Regex exposing (HowMany(All), regex)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        chatView =
            case getActive model of
                Just serverChan ->
                    viewChannel model serverChan

                Nothing ->
                    div [] [ text "nothing." ]
    in
        div []
            [ viewChannelList model
            , chatView
            ]


viewChannelList : Model -> Html Msg
viewChannelList model =
    let
        channelList serverName channels =
            channels
                |> Dict.keys
                |> List.sort
                |> List.map
                    (\name ->
                        li [ onClick (SelectChannel serverName name) ]
                            [ text name ]
                    )

        serverList =
            model.serverInfo
                |> Dict.toList
                -- |> List.sort (\( a, _ ) ( b, _ ) -> a < b)
                |> List.map
                    (\( serverName, serverInfo ) ->
                        li []
                            [ span [ onClick (SelectChannel serverName serverBufferName) ]
                                [ text serverName ]
                            , ul [] (channelList serverName serverInfo.channels)
                            ]
                    )
    in
        aside [ id "channel-list" ] [ ul [] serverList ]


viewChannel : Model -> ( ServerInfo, ChannelInfo ) -> Html Msg
viewChannel model ( server, channel ) =
    div [ id "channel-view" ]
        [ div [ id "channel-header" ]
            [ h3 [] [ text channel.name ]
            , viewTopic channel
            , hr [] []
            ]
        , lazy viewBuffer channel
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


viewBuffer : ChannelInfo -> Html Msg
viewBuffer channel =
    let
        lines =
            channel.buffer
                |> List.reverse
                |> List.map viewLineGroup
    in
        div [ id "buffer-view" ] lines


viewLineGroup : LineGroup -> Html Msg
viewLineGroup group =
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
                        , ( "message-nick-self", False )
                        ]
                    ]
                    [ text group.nick ]
                ]

        messages =
            List.map formatLine group.messages
    in
        div [ class "group" ]
            [ groupHead
            , div [ class "group-messages" ] messages
            ]


formatLine : Line -> Html Msg
formatLine line =
    let
        timeStr =
            Date.format "%H:%M:%S" line.ts

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

        linkify word =
            if String.contains "://" word then
                a [ href word, target "_blank" ] [ text word ]
            else
                text word
    in
        div
            [ title timeStr
            , classList
                [ ( "highlight", String.contains "erik" message ) -- TODO: remove hardcoded nick
                , ( "action", isAction )
                ]
            ]
            (List.map linkify split)
