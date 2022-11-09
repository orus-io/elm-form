module Form.Builder exposing (..)

import Effect exposing (Effect)
import Form exposing (FieldState, Form(..), InputType, getAnyAt, getField)
import Form.Error as Error
import Form.Field as Field exposing (Field, FieldDef(..))
import Form.FieldStack as FieldStack exposing (Stack)
import Form.Internal as Internal
import Form.Validate as Validate exposing (Validation)


type Msg stackMsg
    = FormMsg Form.Msg
    | StackMsg stackMsg


type alias Model customError output stackModel =
    { form : Form customError output
    , stack : stackModel
    }


type Builder validate view model customError sharedMsg output stackModel stackMsg topStackMsg
    = Builder
        { load : List (output -> ( String, Field ))
        , validate : validate
        , view : (stackMsg -> topStackMsg) -> model -> Model customError output stackModel -> view
        , stack : Stack customError output sharedMsg stackModel stackMsg
        }


type alias FormDef customError output sharedMsg model view stackModel stackMsg =
    { init : model -> Maybe output -> ( Model customError output stackModel, Effect sharedMsg (Msg stackMsg) )
    , update : model -> Msg stackMsg -> Model customError output stackModel -> ( Model customError output stackModel, Effect sharedMsg (Msg stackMsg) )
    , subscriptions : model -> Model customError output stackModel -> Sub (Msg stackMsg)
    , view : model -> Model customError output stackModel -> view
    }


onInput : FieldDef output a -> InputType -> FieldState customError a -> a -> Form.Msg
onInput (FieldDef _ toFieldValue _) inputType { path } value =
    Form.Input path inputType (toFieldValue value)


onEmpty : InputType -> FieldState customError a -> Form.Msg
onEmpty inputType { path } =
    Form.Input path inputType Field.EmptyField


type alias FieldViewState customError a stackMsg =
    { state : FieldState customError a
    , onInput : InputType -> a -> Msg stackMsg
    , onEmpty : InputType -> Msg stackMsg
    , onBlur : Msg stackMsg
    , onFocus : Msg stackMsg
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


type alias FieldValidate customError a =
    { valid : Validation customError a
    , validOrEmpty : Validation customError (Maybe a)
    , name : String
    , fromField : Field -> Maybe a
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


andThen : (a -> Validation customError b) -> FieldValidate customError a -> Validation customError b
andThen filter fieldv =
    Validate.field fieldv.name
        (fieldv.fromField
            >> Maybe.map Ok
            >> Maybe.withDefault (Err <| Error.value Error.Empty)
            |> Validate.andThen filter
        )


init :
    { validate : validate, view : view }
    -> Builder validate view model customError sharedMsg output () () topStackMsg
init { validate, view } =
    Builder
        { load = []
        , validate = validate
        , view = \_ _ _ -> view
        , stack = FieldStack.init
        }


field :
    String
    -> FieldDef output a
    -> Builder (FieldValidate customError a -> validate) (FieldViewState customError a stackMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder validate view model customError sharedMsg output stackModel stackMsg topStackMsg
field name (FieldDef fieldload toField fromField) (Builder { validate, view, load, stack }) =
    Builder
        { load =
            case fieldload of
                Just l ->
                    (\d -> ( name, l d |> toField |> Field.value )) :: load

                Nothing ->
                    load
        , validate = validate <| fieldValidate name fromField
        , view =
            \toStackMsg model state ->
                let
                    fieldState =
                        Form.getFieldAs fromField name state.form
                in
                view toStackMsg
                    model
                    state
                    { state = fieldState
                    , onInput = \t value -> FormMsg <| Form.Input fieldState.path t <| toField value
                    , onEmpty = \t -> FormMsg <| Form.Input fieldState.path t Field.EmptyField
                    , onBlur = FormMsg <| Form.Blur fieldState.path
                    , onFocus = FormMsg <| Form.Focus fieldState.path
                    }
        , stack = stack
        }


fieldWithState :
    String
    -> FieldDef output a
    -> (FieldDef output a -> FieldStack.FieldComponent customError a componentModel sharedMsg componentMsg)
    -> Builder (FieldValidate customError a -> validate) (FieldComponentViewState customError a topStackMsg componentModel componentMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
    -> Builder validate view model customError sharedMsg output ( componentModel, stackModel ) (FieldStack.Msg componentMsg stackMsg) topStackMsg
fieldWithState name (FieldDef fieldload toField fromField) component (Builder { validate, view, load, stack }) =
    Builder
        { load =
            case fieldload of
                Just l ->
                    (\d -> ( name, l d |> toField |> Field.value )) :: load

                Nothing ->
                    load
        , validate = validate <| fieldValidate name fromField
        , view =
            \toStackMsg model state ->
                let
                    fieldState =
                        Form.getFieldAs fromField name state.form
                in
                view (FieldStack.PreviousMsg >> toStackMsg)
                    model
                    { form = state.form
                    , stack = Tuple.second state.stack
                    }
                    { state = fieldState
                    , model = Tuple.first state.stack
                    , onInput = \t value -> FormMsg <| Form.Input fieldState.path t <| toField value
                    , onEmpty = \t -> FormMsg <| Form.Input fieldState.path t Field.EmptyField
                    , onBlur = FormMsg <| Form.Blur fieldState.path
                    , onFocus = FormMsg <| Form.Focus fieldState.path
                    , toMsg = FieldStack.CurrentMsg >> toStackMsg >> StackMsg
                    }
        , stack =
            stack
                |> FieldStack.add name fromField (component (FieldDef fieldload toField fromField))
        }



{-
   group :
       String
       -> Builder groupValidate groupView groupModel groupCustomError groupSharedMsg groupOutput groupModel groupStackMsg topStackMsg
       -> Builder (FieldRef -> validate) (FieldComponentViewState customError a topStackMsg componentModel componentMsg -> view) model customError sharedMsg output stackModel stackMsg topStackMsg
       -> Builder validate view model customError sharedMsg output ( componentModel, stackModel ) (FieldStack.Msg componentMsg stackMsg) topStackMsg
   group name group (Builder builder) =
       Builder
           { load =
           , validate = validate <|
           }
-}


finalize :
    Builder (model -> Validation customError output) (model -> view) model customError sharedMsg output stackModel stackMsg stackMsg
    -> FormDef customError output sharedMsg model view stackModel stackMsg
finalize (Builder { validate, view, load, stack }) =
    { init =
        \model initial ->
            let
                formState =
                    Form.initial
                        (initial
                            |> Maybe.map (\data -> load |> List.map (\l -> l data))
                            |> Maybe.withDefault []
                        )
                        (validate model)

                ( stackModel, stackEffect ) =
                    stack.init formState
            in
            ( { form = formState
              , stack = stackModel
              }
            , Effect.map StackMsg stackEffect
            )
    , update =
        \model msg state ->
            case msg of
                FormMsg formMsg ->
                    ( { state
                        | form = Form.update (validate model) formMsg state.form
                      }
                    , Effect.none
                    )

                StackMsg stackMsg ->
                    let
                        ( nextStack, maybeFormMsg, stackEffect ) =
                            stack.update state.form stackMsg state.stack

                        nextForm =
                            case maybeFormMsg of
                                Just formMsg ->
                                    Form.update (validate model) formMsg state.form

                                Nothing ->
                                    state.form
                    in
                    ( { stack = nextStack
                      , form = nextForm
                      }
                    , Effect.map StackMsg stackEffect
                    )
    , subscriptions =
        \_ state ->
            Sub.map StackMsg <| stack.subscriptions state.form state.stack
    , view =
        \model form ->
            view identity model form model
    }
