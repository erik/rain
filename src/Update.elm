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
    case msg of
        SendLine ->
            -- TODO: need to implement this
            ( model, Cmd.none )
        TypeLine str ->
            case getChannel model.currentChannel model of
                Just chan ->
                    let
                        channel_ = { chan | inputLine = str }
                        model_ = setChannel model.currentChannel channel_ model
                    in
                        ( model_, Cmd.none )

                Nothing ->
                    -- TODO: handle this?
                    Debug.log "getChannel was none?"
                    ( model, Cmd.none )

        CreateChannel ( serverName, channelName ) ->
            let
                channel = Model.newChannel
                model_ = setChannel model.currentChannel channel model
            in
                ( model_, Cmd.none )

        SelectChannel ( serverName, channelName ) ->
            case getChannel (serverName, channelName) model of
                Just _ ->
                    ( { model | currentChannel = ( serverName, channelName ) }, Cmd.none )
                _ ->
                    -- TODO: handle this?
                    Debug.log "tried to select a bad channel?"
                    ( model, Cmd.none )



        _ ->
          -- TODO: handle these cases
          ( model, Cmd.none )
