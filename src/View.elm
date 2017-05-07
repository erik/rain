module View exposing (view)

import Date.Format as Date
import Dict
import Dict.Extra exposing (groupBy, mapKeys)
import Html exposing (..)
import Html.Attributes exposing (id, href, class, title)
import Html.Events exposing (onInput, onSubmit, on, keyCode, onClick)
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
                |> Dict.toList
                |> groupBy Tuple.first
                |> Dict.toList
                |> List.map
                    (\( ( sName, cName ), chans ) ->
                        li []
                            [ text sName
                            , ul []
                                (List.map
                                    (\( _, chanInfo ) ->
                                        li []
                                            [ a [ onClick (SelectChannel sName cName) ]
                                                [ text chanInfo.name ]
                                            ]
                                    )
                                    chans
                                )
                            ]
                    )
    in
        div [ id "channelList" ] [ ul [] list ]


viewChannel : Model -> ( ServerInfo, ChannelInfo ) -> Html Msg
viewChannel model ( server, channel ) =
    div []
        [ viewTopic channel
        , input
            [ id "input-line"
            , onInput TypeLine
            , onEnter (SendLine server channel channel.inputLine)
            ]
            []
        , viewBuffer channel
        ]



-- Cribbed from elm-todo


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
    small [] [ text <| Maybe.withDefault "[unset]" channel.topic ]


viewBuffer : ChannelInfo -> Html Msg
viewBuffer channel =
    let
        lines =
            channel.buffer
                -- TODO: |> List.reverse
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
                , b [] [ text group.nick ]
                ]

        messages =
            List.map formatLine group.messages

        -- (\m -> blockquote [] (formatLine m.message))
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

        split =
            Regex.split All (regex "(\\s+)") line.message

        linkify word =
            if String.contains "://" word then
                a [ href word ] [ text word ]
            else
                text word
    in
        blockquote [ title timeStr ]
            (List.map linkify split)
