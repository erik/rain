port module Ports exposing (..)

import Irc exposing (..)
import Model exposing (..)


-- Input ports


port addSavedServer : (ServerMetaData -> msg) -> Sub msg


port onIrcMessage : (( ServerName, ParsedMessage ) -> msg) -> Sub msg


port onIrcConnected : (ServerName -> msg) -> Sub msg



-- Output ports


port sendNotification : ( String, String ) -> Cmd msg


port parseRawLine : ( ServerName, String ) -> Cmd msg


port refreshScrollPosition : Bool -> Cmd msg
