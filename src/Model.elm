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
  { users : Dict String UserInfo
  , topic : Maybe String
  , buffer : Buffer
  , inputLine : String
  }

type alias Model =
  { serverInfo : Dict ServerName ServerInfo
  , channelInfo : Dict ServerChannel ChannelInfo
  , current : ServerChannel
  }

serverBufferName : ChannelName
serverBufferName = ":server:"

getServer : ServerChannel -> Model -> Maybe ServerInfo
getServer (server, _) model =
  D.get server model.serverInfo


newChannel : ChannelInfo
newChannel =
  { users = Dict.empty
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

getChannel : ServerChannel -> Model -> Maybe ChannelInfo
getChannel sc model =
  D.get sc model.channelInfo
