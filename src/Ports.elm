port module Ports exposing (..)

import Model exposing (ServerMetaData, ServerName, Line)


-- Input ports


port addSavedServer : (ServerMetaData -> msg) -> Sub msg


port addScrollback : (( ServerName, Line ) -> msg) -> Sub msg



-- Output ports


port modifyServerStore : ( ServerMetaData, String ) -> Cmd msg


port sendNotification : ( String, String ) -> Cmd msg


port refreshScrollPosition : Bool -> Cmd msg
