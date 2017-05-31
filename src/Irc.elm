module Irc exposing (..)

import Array exposing (..)
import Date
import Model
import Regex exposing (HowMany(All))


type alias ParsedMessage =
    { raw : String
    , time : Float
    , prefix : String
    , command : String
    , params : Array String
    }


type alias User =
    { isServer : Bool
    , nick : String
    , hostname : String
    , realname : String
    }


type Message
    = Unknown ParsedMessage
    | Ping String
    | Privmsg
        { from : User
        , target : String
        , text : String
        , notice : Bool
        }
    | Registered
    | Joined
        { who : User
        , channel : String
        }
    | Parted
        { who : User
        , channel : String
        , reason : Maybe String
        }
    | Topic
        { who : User
        , channel : String
        , text : Maybe String
        }
    | TopicIs { text : String, channel : String }
    | NickList { channel : String, users : List Model.UserInfo }
    | Nick
        { who : User
        , nick : String
        }
    | Kicked
        { who : User
        , whom : String
        , channel : String
        , reason : Maybe String
        }


parseTimeTag : String -> Maybe Date.Date
parseTimeTag tags =
    tags
        |> String.split ";"
        |> List.map (String.split "=")
        |> List.map
            (\split ->
                case split of
                    [ "time", v ] ->
                        Date.fromString v
                            |> Result.toMaybe

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.andThen identity


parsePrefix : String -> User
parsePrefix prefix =
    let
        ( nick, rest ) =
            case String.split "!" prefix of
                [ nick, rest ] ->
                    ( nick, rest )

                _ ->
                    ( "", prefix )

        ( real, host ) =
            case String.split "@" rest of
                [ real, host ] ->
                    ( real, host )

                _ ->
                    ( "", rest )
    in
        { isServer = nick == ""
        , nick = nick
        , hostname = host
        , realname = real
        }


splitMessage : String -> Maybe ParsedMessage
splitMessage line =
    let
        optional re =
            String.concat [ "(?:", re, ")?" ]

        group re =
            String.concat [ "(", re, ")" ]

        -- FIXME: this doesn't support multiple tags for now.
        tag =
            optional "(@(\\w+=\\S+)\\s+)"
                |> group

        prefix =
            optional ":(\\S+)\\s+"
                |> group

        command =
            "(\\S+)"
                |> group

        params =
            "\\s+((?:[^:\\s]\\S*)*)(?:\\s+:?(.*))?"
                |> group

        messageRegex =
            [ "^", tag, prefix, command, params, "$" ]
                |> String.concat
                |> Regex.regex

        matches =
            Regex.find Regex.All messageRegex line
                |> Debug.log "match was this"
                |> List.map .submatches
                |> Debug.log "got these matches"
    in
        case matches of
            [ tags :: prefix :: command :: params ] ->
                Just
                    { raw = line
                    , time = 0
                    , prefix = prefix |> Maybe.withDefault ""
                    , command = command |> Maybe.withDefault ""
                    , params =
                        params
                            |> List.map (Maybe.withDefault "")
                            |> Array.fromList
                    }

            xs ->
                let
                    asdf =
                        Debug.log "what is this" xs
                in
                    Nothing


parse : ParsedMessage -> ( Date.Date, Message )
parse msg =
    let
        ts =
            Date.fromTime msg.time

        _ =
            splitMessage msg.raw
                |> Debug.log "split is"

        handleMessage isNotice =
            let
                user =
                    parsePrefix msg.prefix

                target =
                    get 0 msg.params
                        |> Maybe.map
                            (\x ->
                                if String.startsWith "#" x then
                                    x
                                else
                                    user.nick
                            )

                text =
                    get 1 msg.params
            in
                Privmsg
                    { from = user
                    , target = Maybe.withDefault "" target
                    , text = Maybe.withDefault "" text
                    , notice = isNotice
                    }

        m =
            case msg.command of
                "PING" ->
                    Ping (String.join " " (toList msg.params))

                "PRIVMSG" ->
                    handleMessage False

                "NOTICE" ->
                    handleMessage True

                "JOIN" ->
                    let
                        user =
                            parsePrefix msg.prefix

                        target =
                            get 0 msg.params
                    in
                        Joined
                            { who = user
                            , channel = Maybe.withDefault "what" target
                            }

                "NICK" ->
                    let
                        user =
                            parsePrefix msg.prefix

                        nick =
                            get 0 msg.params
                    in
                        Nick
                            { who = user
                            , nick = Maybe.withDefault "[bug]" nick
                            }

                "332" ->
                    let
                        target =
                            get 1 msg.params

                        topic =
                            get 2 msg.params
                    in
                        TopicIs
                            { text = Maybe.withDefault "what" topic
                            , channel = Maybe.withDefault "what" target
                            }

                "353" ->
                    let
                        specialChars =
                            Regex.regex "[%@~\\+]+"

                        mkUserInfo nickStr =
                            nickStr
                                |> Regex.replace All specialChars (\_ -> "")
                                |> (\nick ->
                                        { nick = nick
                                        , user = ""
                                        , host = ""
                                        , name = ""
                                        }
                                   )

                        users =
                            get 3 msg.params
                                |> Maybe.withDefault ""
                                |> String.words
                                |> List.map mkUserInfo

                        channel =
                            get 2 msg.params
                    in
                        NickList
                            { channel = Maybe.withDefault "wtf" channel
                            , users = users
                            }

                _ ->
                    Unknown msg
    in
        ( ts, m )
