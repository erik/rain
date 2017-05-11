module View exposing (view)

import Date.Format as Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (id, href, class, title, target, value, classList)
import Html.Events exposing (onInput, onSubmit, on, keyCode, onClick)
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
        -- FIXME: lol, wtf
        list =
            model.channelInfo
                |> Dict.keys
                |> List.sort
                |> List.map
                    (\( sName, cName ) ->
                        li []
                            [ text <| sName ++ "/"
                            , a [ onClick (SelectChannel sName cName) ]
                                [ text cName ]
                            ]
                    )
    in
        aside [ id "channel-list" ] [ ul [] list ]


viewChannel : Model -> ( ServerInfo, ChannelInfo ) -> Html Msg
viewChannel model ( server, channel ) =
    div [ id "channel-view" ]
        [ div [ id "channel-header" ]
            [ h3 [] [ text channel.name ]
            , viewTopic channel
            , hr [] []
            ]
        , lazy viewBuffer channel
        , input
            [ id "input-line"
            , onInput TypeLine
            , onEnter (SendLine server channel model.inputLine)
            , value model.inputLine
            ]
            []
        ]


{-| Handle key enter presses.
Cribbed from elm-todo
-}
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


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
