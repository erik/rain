module Irc exposing (..)

import Date
import Model
import Regex exposing (HowMany(All))
import Time


type alias ParsedMessage =
    { raw : String
    , time : Maybe Time.Time
    , user : Model.UserInfo
    , command : String
    , params : List String
    }


parseTimeTag : String -> Maybe Time.Time
parseTimeTag tags =
    tags
        |> String.split ";"
        |> List.filter (String.startsWith "time=")
        |> List.map (String.dropLeft 5)
        |> List.head
        |> Maybe.andThen (Date.fromString >> Result.toMaybe)
        |> Maybe.map Date.toTime


parsePrefix : String -> Model.UserInfo
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
        , host = host
        , real = real
        }


splitMessage : String -> Maybe ParsedMessage
splitMessage line =
    let
        optional re =
            String.concat [ "(?:", re, ")?" ]

        -- FIXME: this doesn't support multiple tags for now.
        tag =
            optional "@(\\w+=\\S+)\\s+"

        prefix =
            optional ":(\\S+)\\s+"

        command =
            "(\\w+)\\s+"

        params =
            optional "([^:]+)\\s*"

        lastParam =
            optional ":(.*?)"

        messageRegex =
            [ "^", tag, prefix, command, params, lastParam, "$" ]
                |> String.concat
                |> Regex.regex

        matches =
            Regex.find Regex.All messageRegex line
                |> List.map .submatches
    in
        case matches of
            [ [ tags, prefix, Just command, params, lastParam ] ] ->
                let
                    finalParam =
                        lastParam
                            |> Maybe.map (\x -> [ x ])
                            |> Maybe.withDefault []

                    splitParams =
                        params
                            |> Maybe.map String.words
                            |> Maybe.withDefault []
                in
                    Just
                        { raw = line
                        , time =
                            tags
                                |> Maybe.andThen parseTimeTag
                        , user =
                            prefix
                                |> Maybe.withDefault ""
                                |> parsePrefix
                        , command = command
                        , params = List.append splitParams finalParam
                        }

            _ ->
                let
                    _ =
                        Debug.log "Failed to parse message" line
                in
                    Nothing
