module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (id) --, class, type_, placeholder, value, autocomplete)

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
  div [ id "channelList" ]
    [ ul []
        [ li [] [ text "text"]
        ]
    ]


viewChannel : Model -> Html Msg
viewChannel model =
  div [] []
