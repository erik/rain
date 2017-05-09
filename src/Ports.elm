port module Ports exposing (..)

import Model exposing (..)
import Irc exposing (..)


port saved_servers : (( ServerName, ServerInfo ) -> msg) -> Sub msg


port send_notification : ( String, String ) -> Cmd msg


port parse_raw : ( ServerName, String ) -> Cmd msg


port irc_messages : (( ServerName, ParsedMessage ) -> msg) -> Sub msg
