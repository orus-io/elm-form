module Enum exposing (Enum, enum)

{-|

@docs Enum, enum

-}


{-| A Enum facilitate listing and converting enumerated values to/from String
-}
type alias Enum option =
    { all : List ( option, String )
    , values : List option
    , toString : option -> String
    , fromString : String -> Maybe option
    }


{-| Builds a Enum given a list of values and their String representation

The String values should have to duplicates.

-}
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
