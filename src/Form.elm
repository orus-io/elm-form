module Form exposing (..)

import Effect exposing (Effect)
import Form.Data as Data exposing (FieldState, getAnyAt, getField)
import Form.InputType as InputType exposing (InputType)
import Form.Error as Error
import Form.Field as Field exposing (Field, FieldDef(..))
import Form.FieldStack as FieldStack exposing (Stack)
import Form.Internal as Internal
import Form.Validate as Validate exposing (Validation)
import List.Extra as List


type Msg stackMsg
    = DataMsg Data.Msg
    | StackMsg stackMsg


type alias BuilderModel customError stackModel =
    { formData : Data.Model customError
    , stack : stackModel
    }


type Builder validate view model customError sharedMsg output stackModel stackMsg topStackMsg
    = Builder
        { load : List (output -> ( String, Field ))
        , validate : validate
        , view : String -> (stackMsg -> topStackMsg) -> model -> BuilderModel customError stackModel -> view
        , stack : Stack customError sharedMsg stackModel stackMsg
        }


type alias Model customError output stackModel =
    { formData : Data.Model customError
    , output: Maybe output
    , stack : stackModel
    }


type alias Form customError output sharedMsg model view stackModel stackMsg =
    { init : model -> Maybe output -> ( Model customError output stackModel, Effect sharedMsg (Msg stackMsg) )
    , update : model -> Msg stackMsg -> Model customError output stackModel -> ( Model customError output stackModel, Effect sharedMsg (Msg stackMsg) )
    , subscriptions : model -> Model customError output stackModel -> Sub (Msg stackMsg)
    , view : model -> Model customError output stackModel -> view
    }


onInput : FieldDef output a -> InputType -> FieldState customError a -> a -> Data.Msg
onInput (FieldDef _ toFieldValue _) inputType { path } value =
    Data.Input path inputType (toFieldValue value)


onEmpty : InputType -> FieldState customError a -> Data.Msg
onEmpty inputType { path } =
    Data.Input path inputType Field.EmptyField


type alias FieldViewState customError a stackMsg =
    { state : FieldState customError a
    , onInput : InputType -> a -> Msg stackMsg
    , onEmpty : InputType -> Msg stackMsg
    , onBlur : Msg stackMsg
    , onFocus : Msg stackMsg
    }


type alias FieldListViewState customError a stackMsg =
    { onAppend : Msg stackMsg
    , items : List (FieldListItemViewState customError a stackMsg)
    }


type alias FieldListItemViewState customError a stackMsg =
    { viewstate : FieldViewState customError a stackMsg
    , onRemove : Msg stackMsg
    }


type alias FieldComponentViewState customError a stackMsg componentModel componentMsg =
    { state : FieldState customError a
    , model : componentModel
    , onInput : InputType -> a -> Msg stackMsg
    , onEmpty : InputType -> Msg stackMsg
    , onBlur : Msg stackMsg
    , onFocus : Msg stackMsg
    , toMsg : componentMsg -> Msg stackMsg
    }


type alias FieldComponentListViewState customError a stackMsg componentModel compomentMsg =
    { onAppend : Msg stackMsg
    , items : List (FieldComponentListItemViewState customError a stackMsg componentModel compomentMsg)
    }


type alias FieldComponentListItemViewState customError a stackMsg componentModel compomentMsg =
    { viewstate : FieldComponentViewState customError a stackMsg componentModel compomentMsg
    , onRemove : Msg stackMsg
    }


type alias FieldValidate customError a =
    { valid : Validation customError a
    , validOrEmpty : Validation customError (Maybe a)
    , name : String
    , fromField : Field -> Maybe a
    }


type alias FieldListValidate customError a =
    { name : String
    , valid : Validation customError (List a)
    , validOrEmpty : Validation customError (List (Maybe a))
    }


type alias GroupListView view stackMsg =
    { onAppend : Msg stackMsg
    , items : List view
    }


fieldValidate : String -> (Field -> Maybe a) -> FieldValidate customError a
fieldValidate fieldname fromField =
    { valid =
        fromField
            >> Maybe.map Ok
            >> Maybe.withDefault (Err <| Error.value Error.Empty)
            |> Validate.field fieldname
    , validOrEmpty =
        fromField
            >> Ok
            |> Validate.field fieldname
    , name = fieldname
    , fromField = fromField
    }


fieldListValidate : String -> (Field -> Maybe a) -> FieldListValidate customError a
fieldListValidate fieldname fromField =
    { valid =
        fromField
            >> Maybe.map Ok
            >> Maybe.withDefault (Err <| Error.value Error.Empty)
            |> Validate.list
            |> Validate.field fieldname
    , validOrEmpty =
        fromField
            >> Ok
            |> Validate.list
            |> Validate.field fieldname
    , name = fieldname
    }


andThen : (a -> Validation customError b) -> FieldValidate customError a -> Validation customError b
andThen filter fieldv =
    Validate.field fieldv.name
        (fieldv.fromField
            >> Maybe.map Ok
            >> Maybe.withDefault (Err <| Error.value Error.Empty)
            |> Validate.andThen filter
        )


init :
    { validate : model -> validate, view : model -> view }
    -> Builder (model -> validate) view model customError sharedMsg output () () topStackMsg
init { validate, view } =
    Builder
        { load = []
        , validate = validate
        , view = \_ _ model _ -> view model
        , stack = FieldStack.init
        }


field :
    String
    -> FieldDef output a
    -> Builder (model -> FieldValidate customError a -> validate) (FieldViewState customError a topStackMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder (model -> validate) view model customError sharedMsg output stackModel stackMsg topStackMsg
field name (FieldDef fieldload toField fromField) (Builder { validate, view, load, stack }) =
    Builder
        { load =
            case fieldload of
                Just l ->
                    (\d -> ( name, l d |> toField |> Field.value )) :: load

                Nothing ->
                    load
        , validate =
            \model ->
                validate model <| fieldValidate name fromField
        , view =
            \path toStackMsg model state ->
                let
                    fieldState =
                        Data.getFieldAs fromField (FieldStack.path path name) state.formData
                in
                view path
                    toStackMsg
                    model
                    state
                    { state = fieldState
                    , onInput = \t value -> DataMsg <| Data.Input fieldState.path t <| toField value
                    , onEmpty = \t -> DataMsg <| Data.Input fieldState.path t Field.EmptyField
                    , onBlur = DataMsg <| Data.Blur fieldState.path
                    , onFocus = DataMsg <| Data.Focus fieldState.path
                    }
        , stack = stack
        }


fieldWithState :
    String
    -> FieldDef output a
    -> (FieldDef output a -> FieldStack.FieldComponent customError a componentModel sharedMsg componentMsg)
    -> Builder (model -> FieldValidate customError a -> validate) (FieldComponentViewState customError a topStackMsg componentModel componentMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder (model -> validate) view model customError sharedMsg output ( componentModel, stackModel ) (FieldStack.Msg componentMsg stackMsg) topStackMsg
fieldWithState name (FieldDef fieldload toField fromField) component (Builder { validate, view, load, stack }) =
    Builder
        { load =
            case fieldload of
                Just l ->
                    (\d -> ( name, l d |> toField |> Field.value )) :: load

                Nothing ->
                    load
        , validate = \model -> validate model <| fieldValidate name fromField
        , view =
            \path toStackMsg model state ->
                let
                    fieldState =
                        Data.getFieldAs fromField (FieldStack.path path name) state.formData
                in
                view path
                    (FieldStack.PreviousMsg >> toStackMsg)
                    model
                    { formData = state.formData
                    , stack = Tuple.second state.stack
                    }
                    { state = fieldState
                    , model = Tuple.first state.stack
                    , onInput = \t value -> DataMsg <| Data.Input fieldState.path t <| toField value
                    , onEmpty = \t -> DataMsg <| Data.Input fieldState.path t Field.EmptyField
                    , onBlur = DataMsg <| Data.Blur fieldState.path
                    , onFocus = DataMsg <| Data.Focus fieldState.path
                    , toMsg = FieldStack.CurrentMsg >> toStackMsg >> StackMsg
                    }
        , stack =
            stack
                |> FieldStack.add name fromField (component (FieldDef fieldload toField fromField))
        }


list :
    String
    -> Maybe (output -> List a)
    -> FieldDef output a
    -> Builder (model -> FieldListValidate customError a -> validate) (FieldListViewState customError a topStackMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder (model -> validate) view model customError sharedMsg output stackModel stackMsg topStackMsg
list name fieldlistload (FieldDef _ toField fromField) (Builder { validate, view, load, stack }) =
    Builder
        { load =
            case fieldlistload of
                Just l ->
                    (\d -> ( name, l d |> List.map (toField >> Field.value) |> Field.list )) :: load

                Nothing ->
                    load
        , validate = \model -> validate model <| fieldListValidate name fromField
        , view =
            \path toStackMsg model state ->
                let
                    fieldStates =
                        Data.getListIndexes (FieldStack.path path name) state.formData
                            |> List.map
                                (\i ->
                                    Data.getFieldAs fromField (FieldStack.itempath path name i) state.formData
                                )
                in
                view path
                    toStackMsg
                    model
                    state
                    { onAppend = DataMsg <| Data.Append name
                    , items =
                        fieldStates
                            |> List.indexedMap
                                (\i fieldState ->
                                    { viewstate =
                                        { state = fieldState
                                        , onInput = \t value -> DataMsg <| Data.Input fieldState.path t <| toField value
                                        , onEmpty = \t -> DataMsg <| Data.Input fieldState.path t Field.EmptyField
                                        , onBlur = DataMsg <| Data.Blur fieldState.path
                                        , onFocus = DataMsg <| Data.Focus fieldState.path
                                        }
                                    , onRemove = DataMsg <| Data.RemoveItem name i
                                    }
                                )
                    }
        , stack = stack
        }


listWithState :
    String
    -> Maybe (output -> List a)
    -> FieldDef output a
    -> (FieldDef output a -> FieldStack.FieldComponent customError a componentModel sharedMsg componentMsg)
    -> Builder (model -> FieldListValidate customError a -> validate) (FieldComponentListViewState customError a topStackMsg componentModel componentMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder (model -> validate) view model customError sharedMsg output ( List componentModel, stackModel ) (FieldStack.Msg ( Int, componentMsg ) stackMsg) topStackMsg
listWithState name fieldlistload (FieldDef fieldload toField fromField) component (Builder { validate, view, load, stack }) =
    Builder
        { load =
            case fieldlistload of
                Just l ->
                    (\d -> ( name, l d |> List.map (toField >> Field.value) |> Field.list )) :: load

                Nothing ->
                    load
        , validate = \model -> validate model <| fieldListValidate name fromField
        , view =
            \path toStackMsg model state ->
                let
                    fieldStates =
                        Data.getListIndexes (FieldStack.path path name) state.formData
                            |> List.map
                                (\i ->
                                    Data.getFieldAs fromField (FieldStack.itempath path name i) state.formData
                                )
                in
                view path
                    (FieldStack.PreviousMsg >> toStackMsg)
                    model
                    { formData = state.formData, stack = Tuple.second state.stack }
                    { onAppend = DataMsg <| Data.Append name
                    , items =
                        fieldStates
                            |> List.indexedMap
                                (\i fieldState ->
                                    { viewstate =
                                        { state = fieldState
                                        , model =
                                            Tuple.first state.stack
                                                |> List.getAt i
                                                |> Maybe.withDefault
                                                    ((component (FieldDef fieldload toField fromField)).init fieldState |> Tuple.first)
                                        , onInput = \t value -> DataMsg <| Data.Input fieldState.path t <| toField value
                                        , onEmpty = \t -> DataMsg <| Data.Input fieldState.path t Field.EmptyField
                                        , onBlur = DataMsg <| Data.Blur fieldState.path
                                        , onFocus = DataMsg <| Data.Focus fieldState.path
                                        , toMsg = Tuple.pair i >> FieldStack.CurrentMsg >> toStackMsg >> StackMsg
                                        }
                                    , onRemove = DataMsg <| Data.RemoveItem (FieldStack.path path name) i
                                    }
                                )
                    }
        , stack =
            stack
                |> FieldStack.addList name fromField (component (FieldDef fieldload toField fromField))
        }


group :
    String
    -> (output -> groupOutput)
    -> Builder (model -> Validation customError groupOutput) groupView model customError sharedMsg groupOutput groupStackModel groupStackMsg topStackMsg
    -> Builder (model -> Validation customError groupOutput -> validate) (groupView -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder (model -> validate) view model customError sharedMsg output ( groupStackModel, stackModel ) (FieldStack.Msg groupStackMsg stackMsg) topStackMsg
group name getGroupData (Builder groupBuilder) (Builder builder) =
    Builder
        { load =
            (\data ->
                let
                    groupData =
                        getGroupData data
                in
                ( name, groupBuilder.load |> List.map (\l -> l groupData) |> Field.group )
            )
                :: builder.load
        , validate =
            \model ->
                builder.validate model <|
                    Validate.field name (groupBuilder.validate model)

        -- view : (stackMsg -> topStackMsg) -> model -> Model customError stackModel -> view
        , view =
            \path toStackMsg model state ->
                let
                    groupView =
                        groupBuilder.view (FieldStack.path path name)
                            (FieldStack.CurrentMsg >> toStackMsg)
                            model
                            { formData = state.formData
                            , stack = Tuple.first state.stack
                            }
                in
                builder.view path
                    (FieldStack.PreviousMsg >> toStackMsg)
                    model
                    { formData = state.formData
                    , stack = Tuple.second state.stack
                    }
                    groupView
        , stack =
            builder.stack
                |> FieldStack.addStack name groupBuilder.stack
        }


groupList :
    String
    -> (output -> List groupOutput)
    -> Builder (model -> Validation customError groupOutput) groupView model customError sharedMsg groupOutput groupStackModel groupStackMsg topStackMsg
    -> Builder (model -> Validation customError (List groupOutput) -> validate) (GroupListView groupView topStackMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder (model -> validate) view model customError sharedMsg output ( List groupStackModel, stackModel ) (FieldStack.Msg ( Int, groupStackMsg ) stackMsg) topStackMsg
groupList name getGroupListData (Builder groupBuilder) (Builder builder) =
    Builder
        { load =
            (\data ->
                ( name
                , getGroupListData data
                    |> List.map
                        (\groupData ->
                            groupBuilder.load |> List.map (\l -> l groupData) |> Field.group
                        )
                    |> Field.list
                )
            )
                :: builder.load
        , validate =
            \model ->
                builder.validate model <|
                    Validate.field name
                        (Validate.list (groupBuilder.validate model))
        , view =
            \path toStackMsg model state ->
                let
                    groupViewList =
                        Data.getListIndexes (FieldStack.path path name) state.formData
                            |> List.map
                                (\i ->
                                    groupBuilder.view
                                        (FieldStack.itempath path name i)
                                        (Tuple.pair i >> FieldStack.CurrentMsg >> toStackMsg)
                                        model
                                        { formData = state.formData
                                        , stack =
                                            Tuple.first state.stack
                                                |> List.getAt i
                                                |> Maybe.withDefault
                                                    (groupBuilder.stack.init (FieldStack.itempath path name i) state.formData
                                                        |> Tuple.first
                                                    )
                                        }
                                )
                in
                builder.view path
                    (FieldStack.PreviousMsg >> toStackMsg)
                    model
                    { formData = state.formData
                    , stack = Tuple.second state.stack
                    }
                    { onAppend = DataMsg <| Data.Append name
                    , items = groupViewList
                    }
        , stack =
            builder.stack
                |> FieldStack.addStackList name groupBuilder.stack
        }


finalize :
    Builder (model -> Validation customError output) view model customError sharedMsg output stackModel stackMsg stackMsg
    -> Form customError output sharedMsg model view stackModel stackMsg
finalize (Builder { validate, view, load, stack }) =
    { init =
        \model initial ->
            let
                (formData, output) =
                    Data.initial
                        (initial
                            |> Maybe.map (\data -> load |> List.map (\l -> l data))
                            |> Maybe.withDefault []
                        )
                        (validate model)

                ( stackModel, stackEffect ) =
                    stack.init "" formData
            in
            ( { formData = formData
              , output = output |> Maybe.withDefault Nothing
              , stack = stackModel
              }
            , Effect.map StackMsg stackEffect
            )
    , update =
        \model msg state ->
            case msg of
                DataMsg formMsg ->
                    let
                        ( newData, newOutput ) =
                            Data.update (validate model) formMsg state.formData
                    in
                    ( { state
                        | formData = newData
                        , output = newOutput |> Maybe.withDefault state.output
                      }
                    , Effect.none
                    )

                StackMsg stackMsg ->
                    let
                        ( nextStack, maybeDataMsg, stackEffect ) =
                            stack.update "" state.formData stackMsg state.stack

                        (nextData, nextOutput) =
                            case maybeDataMsg of
                                Just formMsg ->
                                    Data.update (validate model) formMsg state.formData
                                    |> Tuple.mapSecond (Maybe.withDefault state.output)

                                Nothing ->
                                    (state.formData, state.output)
                    in
                    ( { stack = nextStack
                      , formData = nextData
                      , output = nextOutput
                      }
                    , Effect.map StackMsg stackEffect
                    )
    , subscriptions =
        \_ state ->
            Sub.map StackMsg <| stack.subscriptions "" state.formData state.stack
    , view =
        \model state ->
            view "" identity model { formData = state.formData, stack = state.stack }
    }
