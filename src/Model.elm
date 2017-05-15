module Model exposing (..)

import Date exposing (Date)
import Dict as D
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Time exposing (Time)


type alias ServerName =
    String


type alias ChannelName =
    String


type alias ServerChannel =
    ( ServerName, ChannelName )


serverBufferName : ChannelName
serverBufferName =
    ":server:"


type alias ServerMetaData =
    { proxyHost : String
    , proxyPass : String
    , server : String
    , port_ : String -- TODO: make this an int
    , nick : String
    , pass : String
    , name : String
    }


type alias ServerInfo =
    { socket : String
    , nick : String
    , pass : Maybe String
    , name : String
    , channels : Dict ChannelName ChannelInfo
    , networkChannel : ChannelInfo
    }


type alias Line =
    { ts : Date.Date
    , nick : String
    , message : String
    }


type alias LineGroup =
    { ts : Date.Date
    , nick : String
    , messages : List Line
    }


type alias Buffer =
    List LineGroup


type alias UserInfo =
    { nick : String
    , user : String
    , host : String
    , name : String

    -- TODO: oper, etc?
    }


type alias ChannelInfo =
    { name : String
    , users : Dict String UserInfo
    , topic : Maybe String
    , buffer : Buffer
    }


type alias Model =
    { serverInfo : Dict ServerName ServerInfo
    , current : Maybe ServerChannel
    , inputLine : String
    , currentTime : Time
    , newServerForm : Maybe (Form () ServerMetaData)
    }


newServerValidation : Validation () ServerMetaData
newServerValidation =
    map7 ServerMetaData
        (field "proxyHost" string)
        (field "proxyPass" string)
        (field "server" string)
        (field "port_" string)
        (field "nick" string)
        (field "pass" string)
        (field "name" string)


initialModel : Model
initialModel =
    { serverInfo = Dict.fromList []
    , current = Nothing
    , inputLine = ""
    , currentTime = 0
    , newServerForm = Nothing
    }


getServer : Model -> ServerName -> Maybe ServerInfo
getServer model serverName =
    D.get serverName model.serverInfo


newChannel : String -> ChannelInfo
newChannel name =
    { name = name
    , users = Dict.empty
    , topic = Nothing
    , buffer = []
    }


setChannel : ServerChannel -> ChannelInfo -> Model -> Model
setChannel ( serverName, channelName ) chan model =
    let
        serverInfo =
            getServer model serverName
                |> Maybe.withDefault
                    { socket = "wtf"
                    , nick = "this is a bug"
                    , pass = Nothing
                    , name = "bug"
                    , channels = Dict.empty
                    , networkChannel = newChannel serverBufferName
                    }

        serverInfo_ =
            if channelName == serverBufferName then
                { serverInfo | networkChannel = chan }
            else
                let
                    channels =
                        D.insert channelName chan serverInfo.channels
                in
                    { serverInfo | channels = channels }

        model_ =
            { model | serverInfo = D.insert serverName serverInfo_ model.serverInfo }
    in
        model_


getChannel : Model -> ServerChannel -> Maybe ChannelInfo
getChannel model ( serverName, channelName ) =
    getServer model serverName
        |> Maybe.andThen
            (\info ->
                if channelName == serverBufferName then
                    Just info.networkChannel
                else
                    D.get channelName info.channels
            )


getServerChannel : Model -> ServerChannel -> Maybe ( ServerInfo, ChannelInfo )
getServerChannel model ( sn, cn ) =
    let
        server =
            getServer model sn

        channel =
            getChannel model ( sn, cn )
    in
        Maybe.map2 (\s c -> ( s, c )) server channel


getOrCreateChannel : Model -> ServerChannel -> ChannelInfo
getOrCreateChannel model ( serverName, channelName ) =
    getChannel model ( serverName, channelName )
        |> Maybe.withDefault (newChannel channelName)


getActive : Model -> Maybe ( ServerInfo, ChannelInfo )
getActive model =
    let
        server =
            getActiveServer model

        channel =
            getActiveChannel model
    in
        Maybe.map2 (\s c -> ( s, c )) server channel


getActiveChannel : Model -> Maybe ChannelInfo
getActiveChannel model =
    model.current
        |> Maybe.andThen (getChannel model)


getActiveServer : Model -> Maybe ServerInfo
getActiveServer model =
    model.current
        |> Maybe.andThen (\( s, _ ) -> getServer model s)


appendLine : List LineGroup -> Line -> List LineGroup
appendLine groups line =
    case groups of
        [] ->
            [ { ts = line.ts, nick = line.nick, messages = [ line ] } ]

        hd :: rest ->
            if hd.nick == line.nick then
                { hd | messages = hd.messages ++ [ line ] } :: rest
            else
                [ { ts = line.ts, nick = line.nick, messages = [ line ] }, hd ]
                    ++ rest
