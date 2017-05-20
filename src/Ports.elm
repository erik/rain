port module Ports exposing (..)

import Irc exposing (..)
import Model exposing (..)


-- Input ports


port saved_servers : (ServerMetaData -> msg) -> Sub msg


port irc_messages : (( ServerName, ParsedMessage ) -> msg) -> Sub msg



-- Output ports


port send_notification : ( String, String ) -> Cmd msg


port parse_raw : ( ServerName, String ) -> Cmd msg


port refresh_scroll_position : Bool -> Cmd msg
