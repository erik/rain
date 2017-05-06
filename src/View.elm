module View exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (id, href)
import Html.Events exposing (onInput, onSubmit, on, keyCode, onClick)
import Json.Decode as Json
import Regex exposing (HowMany(All), regex)

import Dict.Extra exposing (groupBy, mapKeys)

import Model exposing (..)
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
      div [] [ viewChannelList model
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
                   (\((sName, cName), chans) ->
                    li [] [ text sName
                          , ul []
                            (List.map
                               (\(_, chanInfo) ->
                                  li [] [
                                     a [ onClick (SelectChannel sName cName) ]
                                       [ text chanInfo.name ]
                                    ])
                               chans)
                          ]
                   )

    in
        div [ id "channelList" ] [ ul [] list ]


viewChannel : Model -> (ServerInfo, ChannelInfo) -> Html Msg
viewChannel model (server, channel) =
  div []
    [ viewTopic channel
    , input [ id "input-line"
            , onInput TypeLine
            , onEnter (SendLine server channel channel.inputLine)
            ] []
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
  small [] [ text <| Maybe.withDefault "[unset]" channel.topic  ]


viewBuffer : ChannelInfo -> Html Msg
viewBuffer channel =
    let
      lines = channel.buffer
            |> List.map viewLine
    in
        ul [ id "buffer-view" ] lines

viewLine : Line -> Html Msg
viewLine line =
  let
    timeStr = toString line.ts
  in
      li []
        [ div [] [
             div [] [ em [] [ text line.nick ]
                    , small [] [ text <| " at " ++ timeStr]
                    ]
            ]
        , div [] [ span [] ( formatLine line.message ) ]]

formatLine : String -> List (Html Msg)
formatLine line =
  let
    split = Regex.split All (regex "(\\s+)") line
    linkify word =
      if String.contains "://" word then
        a [ href word ] [ text word ]
      else
        text word
  in
    List.map linkify split
