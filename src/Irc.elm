port module Irc exposing (..)

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
  | Notice {
        from : User,
        target : String,
        text : String
    }
  | Privmsg {
        from : User,
        target : String,
        text : String
    }
  | Registered
  | Joined {
        who : User,
        channel : String
    }
  | Parted {
        who : User,
        channel : String,
        reason : Maybe String
    }
  | Topic {
        who : User,
        channel : String,
        text : Maybe String
    }
  | Nick {
        who : User,
        nick : String
    }
  | Kicked {
        who : User,
        whom: String,
        channel : String,
        reason : Maybe String
    }


parseUser : String -> User
parseUser prefix =
  { isServer = False
  , nick = "foo"
  , hostname = "host"
  , realname = "foo"
  }

parse : ParsedMessage -> (Date.Date, Message)
parse msg =
  let
    ts = Date.fromTime msg.time
    m = case msg.command of
            "PING" ->
              Ping (String.join " " (toList msg.params))
            "PRIVMSG" ->
              let
                user = parseUser msg.prefix
                target = get 0 msg.params
                text = get 1 msg.params
              in
                  Privmsg { from = user
                          , target = Maybe.withDefault "" target
                          , text = Maybe.withDefault "" text
                          }
            "NOTICE" ->
              let
                user = parseUser msg.prefix
                target = get 0 msg.params
                text = get 1 msg.params
              in
                  Notice { from = user
                         , target = Maybe.withDefault "" target
                         , text = Maybe.withDefault "" text
                         }
            _ ->
              Unknown msg
  in
      (ts, m)
