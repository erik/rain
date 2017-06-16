module Model exposing (..)

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Time exposing (Time)


type alias ServerName =
    String


type alias BufferName =
    String


type alias ServerBuffer =
    ( ServerName, BufferName )


serverBufferName : BufferName
serverBufferName =
    ":server"


type alias ServerMetadata =
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
    , meta : ServerMetadata
    , pass : Maybe String
    , buffers : Dict BufferName BufferInfo
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


{-| to avoid choking on large channels, we wait to uniquify the user names
until we receive the "end of names list" message from the server.

UsersLoaded is nick => last message

-}
type UserList
    = UsersLoading (List String)
    | UsersLoaded (Dict String Time.Time)


type alias BufferInfo =
    { name : String
    , users : UserList
    , topic : Maybe String
    , buffer : Buffer
    , lastChecked : Time.Time
    , isServer : Bool
    }


type alias Model =
    { servers : Dict ServerName ServerInfo
    , current : Maybe ServerBuffer
    , inputLine : String
    , currentTime : Time
    , newServerForm : Maybe (Form () ServerMetadata)
    }


newServerValidation : Validation () ServerMetadata
newServerValidation =
    map8 ServerMetadata
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


newBuffer : String -> BufferInfo
newBuffer name =
    { name = name
    , users = UsersLoading []
    , topic = Nothing
    , buffer = []
    , lastChecked = 0
    , isServer = name == serverBufferName
    }


setNickTimestamp : String -> Time.Time -> BufferInfo -> BufferInfo
setNickTimestamp nick ts buf =
    case buf.users of
        UsersLoading list ->
            buf

        UsersLoaded set ->
            { buf | users = UsersLoaded (Dict.insert nick ts set) }


addNicks : List String -> BufferInfo -> BufferInfo
addNicks nicks buf =
    case buf.users of
        UsersLoading list ->
            { buf | users = UsersLoading (list ++ nicks) }

        UsersLoaded set ->
            let
                users =
                    nicks
                        |> List.map (\nick -> ( nick, 0 ))
                        |> Dict.fromList
                        |> Dict.union set
            in
                { buf | users = UsersLoaded users }


removeNick : String -> BufferInfo -> BufferInfo
removeNick nick buf =
    case buf.users of
        UsersLoading list ->
            { buf | users = UsersLoading (List.filter (\x -> not (x == nick)) list) }

        UsersLoaded set ->
            { buf | users = UsersLoaded (Dict.remove nick set) }


getServer : Model -> ServerName -> Maybe ServerInfo
getServer model serverName =
    Dict.get serverName model.servers


setBuffer : ServerInfo -> BufferInfo -> Model -> Model
setBuffer serverInfo buf model =
    let
        name_ =
            String.toLower buf.name

        serverInfo_ =
            let
                buffers =
                    Dict.insert name_ buf serverInfo.buffers
            in
                { serverInfo | buffers = buffers }
    in
        { model | servers = Dict.insert serverInfo.meta.name serverInfo_ model.servers }


getBuffer : ServerInfo -> BufferName -> Maybe BufferInfo
getBuffer serverInfo bufferName =
    Dict.get (String.toLower bufferName) serverInfo.buffers


getServerBuffer : Model -> ServerBuffer -> Maybe ( ServerInfo, BufferInfo )
getServerBuffer model ( sn, bn ) =
    let
        server =
            getServer model sn

        buffer =
            server
                |> Maybe.andThen (\server -> getBuffer server bn)
    in
        Maybe.map2 (,) server buffer


getOrCreateBuffer : ServerInfo -> BufferName -> BufferInfo
getOrCreateBuffer serverInfo bufferName =
    getBuffer serverInfo bufferName
        |> Maybe.withDefault (newBuffer bufferName)


getActive : Model -> Maybe ( ServerInfo, BufferInfo )
getActive model =
    model.current |> Maybe.andThen (getServerBuffer model)


getActiveBuffer : Model -> Maybe BufferInfo
getActiveBuffer model =
    getActive model |> Maybe.map Tuple.second


getActiveServer : Model -> Maybe ServerInfo
getActiveServer model =
    getActive model |> Maybe.map Tuple.first


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
