module Model exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Dict as D

type alias ServerName = String
type alias ChannelName = String

type alias ServerChannel = (ServerName, ChannelName)

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

type alias Buffer = List Line

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
serverBufferName = ":server:"

getServer : Model -> ServerChannel -> Maybe ServerInfo
getServer model (server, _) =
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
    channelInfo = D.insert sc chan model.channelInfo
  in
      { model | channelInfo = channelInfo }

getChannel : Model -> ServerChannel -> Maybe ChannelInfo
getChannel model sc =
  D.get sc model.channelInfo


getActiveChannel : Model -> Maybe ChannelInfo
getActiveChannel model =
  model.current
    |> Maybe.andThen (getChannel model)


getActiveServer : Model -> Maybe ServerInfo
getActiveServer model =
  model.current
    |> Maybe.andThen (getServer model)
