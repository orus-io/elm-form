module Form.Builder exposing (..)

import Effect exposing (Effect)
import Form exposing (FieldState, Form(..), InputType, Msg(..), getAnyAt, getField)
import Form.Field as Field exposing (Field, FieldDef(..))
import Form.Validate as Validate exposing (Validation)
import Internal


type Builder validate view model customError output
    = Builder
        { load : List (output -> ( String, Field ))
        , validate : validate
        , view : model -> Form customError output -> view
        }


type alias FormDef customError output sharedMsg model view =
    { init : model -> Maybe output -> Form customError output
    , update : model -> Msg -> Form customError output -> ( Form customError output, Effect sharedMsg Msg )
    , subscriptions : model -> Form customError output -> Sub Msg
    , view : model -> Form customError output -> view
    }


type alias FieldRef =
    Internal.FieldRef


type alias FieldViewState customError a =
    { state : FieldState customError a
    , onInput : InputType -> a -> Msg
    , onBlur : Msg
    , onFocus : Msg
    }


init : { validate : validate, view : view } -> Builder validate view model customError output
init { validate, view } =
    Builder
        { load = []
        , validate = validate
        , view = \_ _ -> view
        }


field :
    String
    -> FieldDef output a
    -> Builder (FieldRef -> validate) (FieldViewState customError a -> view) model customError output
    -> Builder validate view model customError output
field name (FieldDef fieldload toField toFieldValue) (Builder { validate, view, load }) =
    Builder
        { load =
            case fieldload of
                Just l ->
                    (\d -> ( name, l d |> toField |> Field.value )) :: load

                Nothing ->
                    load
        , validate = validate <| Internal.FieldRef name
        , view =
            \model (F formModel) ->
                let
                    state =
                        getField (getAnyAt toFieldValue)
                            name
                            (F formModel)
                in
                view model
                    (F formModel)
                    { state = state
                    , onInput = \t value -> Input state.path t <| toField value
                    , onBlur = Blur state.path
                    , onFocus = Focus state.path
                    }
        }


finalize :
    Builder (model -> Validation customError output) (model -> view) model customError output
    -> FormDef customError output sharedMsg model view
finalize (Builder { validate, view, load }) =
    { init =
        \model ->
            Maybe.map (\data -> Form.initial (load |> List.map (\l -> l data)) <| validate model)
                >> Maybe.withDefault (Form.initial [] <| validate model)
    , update =
        \model msg formdata ->
            ( Form.update (validate model) msg formdata, Effect.none )
    , subscriptions =
        \_ _ ->
            Sub.none
    , view =
        \model form ->
            view model form model
    }
