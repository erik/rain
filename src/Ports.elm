port module Ports exposing (..)

import Model exposing (BufferName, ServerMetaData, ServerName, Line)


-- Input ports


port addSavedServer : (ServerMetaData -> msg) -> Sub msg


port receiveScrollback : (( ServerName, BufferName, Line ) -> msg) -> Sub msg



-- Output ports


port modifyServerStore : ( ServerMetaData, String ) -> Cmd msg


port clearScrollback : ( ServerName, BufferName ) -> Cmd msg


port requestScrollback : ServerName -> Cmd msg


port saveScrollback : ( ServerName, BufferName, Line ) -> Cmd msg


port sendNotification : ( String, String ) -> Cmd msg


port refreshScrollPosition : Bool -> Cmd msg
