module Form.Data exposing (..)

import Dict exposing (Dict)
import Form.Error as Error exposing (Error)
import Form.Field as Field exposing (Field, FieldDef(..), FieldValue)
import Form.InputType as InputType exposing (InputType)
import Form.Tree as Tree
import Set exposing (Set)


getAnyAt : (Field -> Maybe a) -> String -> Model e -> Maybe a
getAnyAt toFieldValue name model =
    getFieldAt name model |> Maybe.andThen toFieldValue


{-| Form to embed in your model. Type parameters are:

  - `customError` - a custom error type to extend built-in errors (set to `()` if you don't need it)
  - `output` - the type of the validation output.

-}
type Form customError output
    = F (Model customError) (Maybe output)


{-| Private
-}
type alias Model customError =
    { fields : Field
    , focus : Maybe String
    , dirtyFields : Set String
    , changedFields : Set String
    , originalValues : Dict String (Maybe FieldValue)
    , isSubmitted : Bool
    , errors : List ( String, List customError )
    }


type alias Validation customError output =
    Model customError -> Result (List ( String, List customError )) output


{-| Initial form state. See `Form.Field` for initial fields, and `Form.Validate` for validation.
-}
initial : List ( String, Field ) -> Validation e output -> ( Model e, Maybe (Maybe output) )
initial initialFields validation =
    let
        model =
            { fields = Tree.group initialFields
            , focus = Nothing
            , dirtyFields = Set.empty
            , changedFields = Set.empty
            , originalValues = Dict.empty
            , isSubmitted = False
            , errors = []
            }
    in
    updateValidateToForm validation model


{-| Field state containing all necessary data for view and update,
can be retrived with `Form.getFieldAsString` or `Form.getFieldAsBool`.

  - `path` - qualified path of the field in the form, with dots for nested fields (`field.subfield`)
  - `value` - a `Maybe` of the requested type
  - `error` - a `Maybe` of the field error
  - `liveError` - same but with added logic for live validation
    (see [`getLiveErrorAt`](https://github.com/etaque/elm-form/blob/master/src/Form.elm) impl)
  - `isDirty` - if the field content has been changed since last validation
  - `isChanged` - if the field value has changed since last init/reset
  - `hasFocus` - if the field is currently focused

-}
type alias FieldState e a =
    { path : String
    , value : Maybe a
    , error : List e
    , liveError : List e
    , isDirty : Bool
    , isChanged : Bool
    , hasFocus : Bool
    }


{-| Get field state at path, with value as a `String`.
-}
getFieldAsString : String -> Model e -> FieldState e String
getFieldAsString =
    getField getStringAt


{-| Get field state at path, with value as a `Bool`.
-}
getFieldAsBool : String -> Model e -> FieldState e Bool
getFieldAsBool =
    getField getBoolAt


getFieldAs : (Field -> Maybe a) -> String -> Model e -> FieldState e a
getFieldAs fromField =
    getField (getAnyAt fromField)


getField : (String -> Model e -> Maybe a) -> String -> Model e -> FieldState e a
getField getValue path form =
    { path = path
    , value = getValue path form
    , error = getErrorAt path form
    , liveError = getLiveErrorAt path form
    , isDirty = isDirtyAt path form
    , isChanged = isChangedAt path form
    , hasFocus = getFocus form == Just path
    }


{-| return a list of indexes so one can build qualified names of fields in list.
-}
getListIndexes : String -> Model e -> List Int
getListIndexes path model =
    let
        length =
            getFieldAt path model
                |> Maybe.map (Tree.asList >> List.length)
                |> Maybe.withDefault 0
    in
    List.range 0 (length - 1)


{-| Form messages for `update`.
-}
type Msg
    = NoOp
    | Focus String
    | Blur String
    | Input String InputType FieldValue
    | Append String
    | RemoveItem String Int
    | Submit
    | Validate
    | Reset (List ( String, Field ))


{-| Update form state with the given message
-}
update : Validation e output -> Msg -> Model e -> ( Model e, Maybe (Maybe output) )
update validation msg model =
    case msg of
        NoOp ->
            ( model, Nothing )

        Focus name ->
            let
                newModel =
                    { model | focus = Just name }
            in
            ( newModel, Nothing )

        Blur name ->
            let
                newDirtyFields =
                    Set.remove name model.dirtyFields

                newModel =
                    { model | focus = Nothing, dirtyFields = newDirtyFields }
            in
            updateValidateToForm validation newModel

        Input name inputType fieldValue ->
            let
                newFields =
                    setFieldAt name (Tree.Value fieldValue) model

                isDirty =
                    case inputType of
                        InputType.Text ->
                            True

                        InputType.Textarea ->
                            True

                        _ ->
                            False

                newDirtyFields =
                    if isDirty then
                        Set.insert name model.dirtyFields

                    else
                        model.dirtyFields

                ( newChangedFields, newOriginalValues ) =
                    if Set.member name model.changedFields then
                        let
                            storedValue =
                                Dict.get name model.originalValues
                                    |> Maybe.withDefault Nothing

                            shouldBeNothing v =
                                case v of
                                    Field.String "" ->
                                        True

                                    Field.Bool False ->
                                        True

                                    _ ->
                                        False

                            sameAsOriginal =
                                case storedValue of
                                    Just v ->
                                        v == fieldValue

                                    Nothing ->
                                        shouldBeNothing fieldValue

                            changedFields =
                                if sameAsOriginal then
                                    Set.remove name model.changedFields

                                else
                                    model.changedFields
                        in
                        ( changedFields, model.originalValues )

                    else
                        let
                            originalValue =
                                getFieldAt name model |> Maybe.andThen Tree.asValue
                        in
                        ( Set.insert name model.changedFields, Dict.insert name originalValue model.originalValues )

                newModel =
                    { model
                        | fields = newFields
                        , dirtyFields = newDirtyFields
                        , changedFields = newChangedFields
                        , originalValues = newOriginalValues
                    }
            in
            updateValidateToForm validation newModel

        Append listName ->
            let
                listFields =
                    getFieldAt listName model
                        |> Maybe.map Tree.asList
                        |> Maybe.withDefault []

                newListFields =
                    listFields ++ [ Tree.Value Field.EmptyField ]

                newModel =
                    { model
                        | fields = setFieldAt listName (Tree.List newListFields) model
                    }
            in
            ( newModel, Nothing )

        RemoveItem listName index ->
            let
                listFields =
                    getFieldAt listName model
                        |> Maybe.map Tree.asList
                        |> Maybe.withDefault []

                fieldNamePattern =
                    listName ++ String.fromInt index

                filterChangedFields =
                    Set.filter (not << String.startsWith fieldNamePattern)

                filterOriginalValue =
                    Dict.filter (\c _ -> not <| String.startsWith fieldNamePattern c)

                newListFields =
                    List.take index listFields ++ List.drop (index + 1) listFields

                newModel =
                    { model
                        | fields = setFieldAt listName (Tree.List newListFields) model
                        , changedFields = filterChangedFields model.changedFields
                        , originalValues = filterOriginalValue model.originalValues
                    }
            in
            updateValidateToForm validation newModel

        Submit ->
            let
                ( validatedModel, validatedOutput ) =
                    updateValidateToForm validation model
            in
            ( { validatedModel | isSubmitted = True }, validatedOutput )

        Validate ->
            updateValidateToForm validation model

        Reset fields ->
            let
                newModel =
                    { model
                        | fields = Tree.group fields
                        , dirtyFields = Set.empty
                        , changedFields = Set.empty
                        , originalValues = Dict.empty
                        , isSubmitted = False
                    }
            in
            updateValidateToForm validation newModel


updateValidateToForm : Validation e o -> Model e -> ( Model e, Maybe (Maybe o) )
updateValidateToForm validation model =
    updateValidate validation model
        |> Tuple.mapSecond Just


updateValidate : Validation e o -> Model e -> ( Model e, Maybe o )
updateValidate validation model =
    case validation model of
        Ok output ->
            ( { model
                | errors = []
              }
            , Just output
            )

        Err errors ->
            ( { model
                | errors =
                    errors
              }
            , Nothing
            )


getFieldAt : String -> Model e -> Maybe Field
getFieldAt qualifiedName model =
    Tree.getAtPath qualifiedName model.fields


getStringAt : String -> Model e -> Maybe String
getStringAt name model =
    getFieldAt name model |> Maybe.andThen Field.asString


getBoolAt : String -> Model e -> Maybe Bool
getBoolAt name model =
    getFieldAt name model |> Maybe.andThen Field.asBool


setFieldAt : String -> Field -> Model e -> Field
setFieldAt path field_ model =
    Tree.setAtPath path field_ model.fields


{-| Get form submission state. Useful to show errors on unchanged fields.
-}
isSubmitted : Model e -> Bool
isSubmitted model =
    model.isSubmitted


mergeErrorList : List ( String, e ) -> List ( String, List e )
mergeErrorList =
    List.foldl
        (\( key, err ) out ->
            case List.filter (Tuple.first >> (==) key) out |> List.head of
                Just ( _, errs ) ->
                    ( key, err :: errs ) :: out

                Nothing ->
                    ( key, [ err ] ) :: out
        )
        []


{-| Get list of errors on qualified paths.
-}
getErrors : Model e -> List ( String, List e )
getErrors { errors } =
    errors


getErrorAt : String -> Model e -> List e
getErrorAt path { errors } =
    List.filter (Tuple.first >> (==) path) errors
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault []


getLiveErrorAt : String -> Model e -> List e
getLiveErrorAt name form =
    if isSubmitted form || (isChangedAt name form && not (isDirtyAt name form)) then
        getErrorAt name form

    else
        []


isChangedAt : String -> Model e -> Bool
isChangedAt qualifiedName model =
    Set.member qualifiedName model.changedFields


isDirtyAt : String -> Model e -> Bool
isDirtyAt qualifiedName model =
    Set.member qualifiedName model.dirtyFields


{-| Return currently focused field, if any.
-}
getFocus : Model e -> Maybe String
getFocus model =
    model.focus


{-| Get set of changed fields.
-}
getChangedFields : Model e -> Set String
getChangedFields model =
    model.changedFields
