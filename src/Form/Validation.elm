module Form.Validation exposing (..)


type Validation error raw parsed
    = Validation
        { path : Maybe String
        , raw : Maybe raw
        , parsed : Maybe parsed
        , errors : List ( String, List error )
        }


type alias Combined error parsed =
    Validation error Never parsed


field : String -> Maybe a -> Validation customError a a
field path raw =
    Validation
        { path = Just path
        , raw = raw
        , parsed = raw
        , errors = []
        }


rawValue : Validation error raw parsed -> Maybe raw
rawValue (Validation { raw }) =
    raw


getErrors : Validation error raw parsed -> List ( String, List error )
getErrors (Validation { errors }) =
    errors


asResult : Validation error raw a -> Result (List ( String, List error )) a
asResult (Validation { parsed, errors }) =
    case ( parsed, errors ) of
        ( Just value, [] ) ->
            Ok value

        _ ->
            Err errors


succeed : a -> Validation error Never a
succeed value =
    Validation
        { path = Nothing
        , raw = Nothing
        , parsed = Just value
        , errors = []
        }


list : List (Validation error raw a) -> Combined error (List a)
list =
    List.foldr
        (\(Validation { parsed, errors }) out ->
            { out
                | parsed = Maybe.map2 (::) parsed out.parsed
                , errors = List.append errors out.errors
            }
        )
        { path = Nothing
        , raw = Nothing
        , parsed = Just []
        , errors = []
        }
        >> Validation


andThen :
    (parsed -> Validation error raw2 mapped)
    -> Validation error raw parsed
    -> Validation error raw mapped
andThen fn (Validation { path, raw, parsed, errors }) =
    case parsed of
        Nothing ->
            Validation { path = path, raw = raw, parsed = Nothing, errors = errors }

        Just value ->
            let
                (Validation mappedV) =
                    fn value
            in
            Validation
                { path = path
                , raw = raw
                , parsed = mappedV.parsed
                , errors = List.append errors mappedV.errors
                }


andMap :
    Validation error raw parsed
    -> Validation error raw2 (parsed -> mapped)
    -> Combined error mapped
andMap (Validation { path, parsed, errors }) (Validation mapper) =
    Validation
        { path =
            if path == mapper.path then
                path

            else
                Nothing
        , raw = Nothing
        , parsed =
            Maybe.map2
                (\p m ->
                    m p
                )
                parsed
                mapper.parsed
        , errors = List.append errors mapper.errors
        }


map : (parsed -> mapped) -> Validation error raw parsed -> Validation error raw mapped
map fn (Validation { raw, path, parsed, errors }) =
    Validation
        { raw = raw
        , path = path
        , parsed = parsed |> Maybe.map fn
        , errors = errors
        }


map2 :
    (a -> b -> mapped)
    -> Validation error rawa a
    -> Validation error rawb b
    -> Combined error mapped
map2 fn v1 v2 =
    succeed fn
        |> andMap v1
        |> andMap v2


map3 :
    (a -> b -> c -> mapped)
    -> Validation error rawa a
    -> Validation error rawb b
    -> Validation error rawb c
    -> Combined error mapped
map3 fn v1 v2 v3 =
    succeed fn
        |> andMap v1
        |> andMap v2
        |> andMap v3
