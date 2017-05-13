module Irc exposing (..)

import Model
import Regex exposing (HowMany(All))
import Array exposing (..)
import Date


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
    | Notice
        { from : User
        , target : String
        , text : String
        }
    | Privmsg
        { from : User
        , target : String
        , text : String
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


parseUser : String -> User
parseUser prefix =
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


parse : ParsedMessage -> ( Date.Date, Message )
parse msg =
    let
        ts =
            Date.fromTime msg.time

        m =
            case msg.command of
                "PING" ->
                    Ping (String.join " " (toList msg.params))

                "PRIVMSG" ->
                    let
                        user =
                            parseUser msg.prefix

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
                            }

                "NOTICE" ->
                    let
                        user =
                            parseUser msg.prefix

                        target =
                            get 0 msg.params

                        text =
                            get 1 msg.params
                    in
                        Notice
                            { from = user
                            , target = Maybe.withDefault "" target
                            , text = Maybe.withDefault "" text
                            }

                "JOIN" ->
                    let
                        user =
                            parseUser msg.prefix

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
                            parseUser msg.prefix

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
