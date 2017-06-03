port module Ports exposing (..)

import Model exposing (ServerMetaData)


-- Input ports


port addSavedServer : (ServerMetaData -> msg) -> Sub msg



-- Output ports


port saveServer : ServerMetaData -> Cmd msg


port sendNotification : ( String, String ) -> Cmd msg


port refreshScrollPosition : Bool -> Cmd msg
