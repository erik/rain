module Model exposing (..)

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Set exposing (Set)
import Time exposing (Time)


type alias ServerName =
    String


{-| TODO: rename to buffer
-}
type alias ChannelName =
    String


type alias ServerChannel =
    ( ServerName, ChannelName )


serverBufferName : ChannelName
serverBufferName =
    "::server::"


type alias ServerMetaData =
    { proxyHost : String
    , proxyPass : String
    , server : String
    , port_ : String -- TODO: make this an int
    , nick : String
    , pass : String
    , name : String
    , saveScrollback : Bool
    }


type alias ServerInfo =
    { socket : String
    , nick : String
    , pass : Maybe String
    , name : String
    , meta : ServerMetaData
    , channels : Dict ChannelName ChannelInfo
    , networkChannel : ChannelInfo
    }


type alias Line =
    { ts : Time.Time
    , nick : String
    , message : String
    }


type alias LineGroup =
    { ts : Time.Time
    , nick : String
    , messages : List Line
    }


type alias Buffer =
    List LineGroup


type alias UserInfo =
    { nick : String
    , host : String
    , real : String
    , isServer : Bool
    }


type alias ChannelInfo =
    { name : String
    , users : Set String
    , topic : Maybe String
    , buffer : Buffer
    , lastChecked : Time.Time
    , isServer : Bool
    }


type alias Model =
    { servers : Dict ServerName ServerInfo
    , current : Maybe ServerChannel
    , inputLine : String
    , currentTime : Time
    , newServerForm : Maybe (Form () ServerMetaData)
    }


newServerValidation : Validation () ServerMetaData
newServerValidation =
    map8 ServerMetaData
        (field "proxyHost" string)
        (field "proxyPass" string)
        (field "server" string)
        (field "port_" string)
        (field "nick" string)
        (field "pass" string)
        (field "name" string)
        (field "saveScrollback" bool)


initialModel : Model
initialModel =
    { servers = Dict.empty
    , current = Nothing
    , inputLine = ""
    , currentTime = 0
    , newServerForm = Nothing
    }


newChannel : String -> ChannelInfo
newChannel name =
    { name = name
    , users = Set.empty
    , topic = Nothing
    , buffer = []
    , lastChecked = 0
    , isServer = False
    }


getServer : Model -> ServerName -> Maybe ServerInfo
getServer model serverName =
    Dict.get serverName model.servers


setChannel : ServerInfo -> ChannelInfo -> Model -> Model
setChannel serverInfo chan model =
    let
        serverInfo_ =
            if chan.isServer || chan.name == serverBufferName then
                { serverInfo | networkChannel = { chan | isServer = True } }
            else
                let
                    channels =
                        Dict.insert (String.toLower chan.name) chan serverInfo.channels
                in
                    { serverInfo | channels = channels }
    in
        { model | servers = Dict.insert serverInfo.name serverInfo_ model.servers }


getChannel : ServerInfo -> ChannelName -> Maybe ChannelInfo
getChannel serverInfo channelName =
    if channelName == serverBufferName then
        Just serverInfo.networkChannel
    else
        Dict.get (String.toLower channelName) serverInfo.channels


getServerChannel : Model -> ServerChannel -> Maybe ( ServerInfo, ChannelInfo )
getServerChannel model ( sn, cn ) =
    let
        server =
            getServer model sn

        channel =
            server
                |> Maybe.andThen (\server -> getChannel server cn)
    in
        Maybe.map2 (,) server channel


getOrCreateChannel : ServerInfo -> ChannelName -> ChannelInfo
getOrCreateChannel serverInfo channelName =
    getChannel serverInfo channelName
        |> Maybe.withDefault (newChannel channelName)


getActive : Model -> Maybe ( ServerInfo, ChannelInfo )
getActive model =
    model.current
        |> Maybe.andThen (getServerChannel model)


getActiveChannel : Model -> Maybe ChannelInfo
getActiveChannel model =
    getActive model
        |> Maybe.map Tuple.second


getActiveServer : Model -> Maybe ServerInfo
getActiveServer model =
    getActive model
        |> Maybe.map Tuple.first


appendLine : List LineGroup -> Line -> List LineGroup
appendLine groups line =
    case groups of
        [] ->
            [ { ts = line.ts
              , nick = line.nick
              , messages = [ line ]
              }
            ]

        hd :: rest ->
            if hd.nick == line.nick then
                { hd | messages = line :: hd.messages } :: rest
            else
                List.take 1000 rest
                    |> List.append
                        [ { ts = line.ts
                          , nick = line.nick
                          , messages = [ line ]
                          }
                        , hd
                        ]
