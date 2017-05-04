module View exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (id) --, class, type_, placeholder, value, autocomplete)

import Dict.Extra exposing (groupBy, mapKeys)

import Model exposing (..)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
  div []
    [ viewChannelList model
    , viewChannel model
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

viewChannel : Model -> Html Msg
viewChannel model =
  div [] []
