module Update exposing (Msg(..), update)

import Debug
import Dict as D

import Model exposing (..)


type Msg
  = SendLine
  | TypeLine String
  | CreateChannel ( ServerName, ChannelName )
  | SelectChannel ( ServerName, ChannelName )
  | CloseChannel ( ServerName, ChannelName )
  | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    current = model.currentChannel
    server = getServer current model
    channel = getChannel current model
  in
      case msg of
          SendLine ->
            -- TODO: need to implement this
            ( model, Cmd.none )
          TypeLine str ->
            case channel of
                Just chan ->
                  let
                    channel_ = { chan | inputLine = str }
                    model_ = { model | channelInfo = D.insert current channel_ model.channelInfo }
                  in
                    ( model_, Cmd.none )
                Nothing ->
                  -- TODO: handle this?
                  Debug.log "getChannel was none?"
                  ( model, Cmd.none )

          CreateChannel ( serverName, channelName ) ->
            let
              channel = newChannel

            in
                ( model, Cmd.none )



          _ ->
            -- TODO: handle these cases
            ( model, Cmd.none )
