port module Ports exposing (..)

import Model exposing (ChannelName, ServerMetaData, ServerName, Line)


-- Input ports


port addSavedServer : (ServerMetaData -> msg) -> Sub msg


port receiveScrollback : (( ServerName, ChannelName, Line ) -> msg) -> Sub msg



-- Output ports


port modifyServerStore : ( ServerMetaData, String ) -> Cmd msg


port requestScrollback : ServerName -> Cmd msg


port saveScrollback : ( ServerName, ChannelName, Line ) -> Cmd msg


port sendNotification : ( String, String ) -> Cmd msg


port refreshScrollPosition : Bool -> Cmd msg
