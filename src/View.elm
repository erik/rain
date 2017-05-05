module View exposing (view)

import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (id) --, class, type_, placeholder, value, autocomplete)
import Html.Events exposing (onInput)

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
            h1 [] [ text "nothing has happened yet" ]
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
                   (\((sName, cName), chans) ->
                    li [] [ text sName
                          , ul [] (List.map
                                       (\(_, chanInfo) ->
                                            li [] [ text chanInfo.name ])
                                       chans)
                          ]
                   )

    in
        div [ id "channelList" ] [ ul [] list ]


viewChannel : Model -> (ServerInfo, ChannelInfo) -> Html Msg
viewChannel model (server, channel) =
  div []
    [ viewTopic channel
    , viewBuffer channel
    , input [ onInput TypeLine ] []
    ]


viewTopic : ChannelInfo -> Html Msg
viewTopic channel =
  span [] [ text <| Maybe.withDefault "[unset]" channel.topic  ]


viewBuffer : ChannelInfo -> Html Msg
viewBuffer channel =
    let
      lines = channel.buffer
            |> List.map viewLine
    in
        div [] lines

viewLine : Line -> Html Msg
viewLine line =
  div [] [ div [] [ text line.nick ++ " at " ++ (Date.toString line.ts)
                  ]
         , div [] [ p [] [ text line.message ]]]
