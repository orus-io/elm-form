module Enum exposing (..)


type alias Enum option =
    { all : List ( option, String )
    , values : List option
    , toString : option -> String
    , fromString : String -> Maybe option
    }


enum : List ( option, String ) -> Enum option
enum list =
    { all = list
    , values = List.map Tuple.first list
    , toString =
        \value ->
            List.foldl
                (\( o, s ) r ->
                    if o == value then
                        s

                    else
                        r
                )
                ""
                list
    , fromString =
        \value ->
            List.foldl
                (\( o, s ) r ->
                    if s == value then
                        Just o

                    else
                        r
                )
                Nothing
                list
    }
