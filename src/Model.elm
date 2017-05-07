port module Model exposing (..)

import Date exposing (Date)
import Dict as D
import Dict exposing (Dict)


type alias ServerName =
    String


type alias ChannelName =
    String


type alias ServerChannel =
    ( ServerName, ChannelName )


type alias ServerInfo =
    { socket : String
    , nick : String
    , pass : Maybe String
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
    , inputLine : String
    }


type alias Model =
    { serverInfo : Dict ServerName ServerInfo
    , channelInfo : Dict ServerChannel ChannelInfo
    , current : Maybe ServerChannel
    }


initialModel : Model
initialModel =
    { serverInfo = Dict.fromList []
    , channelInfo = Dict.fromList []
    , current = Nothing
    }


serverBufferName : ChannelName
serverBufferName =
    ":server:"


getServer : Model -> ServerChannel -> Maybe ServerInfo
getServer model ( server, _ ) =
    D.get server model.serverInfo


newChannel : String -> ChannelInfo
newChannel name =
    { name = name
    , users = Dict.empty
    , topic = Nothing
    , buffer = []
    , inputLine = ""
    }


setChannel : ServerChannel -> ChannelInfo -> Model -> Model
setChannel sc chan model =
    let
        channelInfo =
            D.insert sc chan model.channelInfo
    in
        { model | channelInfo = channelInfo }


getChannel : Model -> ServerChannel -> Maybe ChannelInfo
getChannel model sc =
    D.get sc model.channelInfo


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
        |> Maybe.andThen (getServer model)


appendLine : List LineGroup -> Line -> List LineGroup
appendLine groups line =
    case groups of
        [] ->
            { ts = line.ts, nick = line.nick, messages = [ line ] } :: []

        hd :: rest ->
            if hd.nick == line.nick then
                { hd | messages = line :: hd.messages } :: rest
            else
                [ { ts = line.ts, nick = line.nick, messages = [ line ] }, hd ]
                    ++ rest



-- ports


port saved_servers : (( ServerName, ServerInfo ) -> msg) -> Sub msg
