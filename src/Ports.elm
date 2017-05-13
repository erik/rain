port module Ports exposing (..)

import Irc exposing (..)
import Model exposing (..)


port saved_servers : (( ServerName, ServerMetaData ) -> msg) -> Sub msg


port send_notification : ( String, String ) -> Cmd msg


port parse_raw : ( ServerName, String ) -> Cmd msg


port irc_messages : (( ServerName, ParsedMessage ) -> msg) -> Sub msg
