module Form.FieldStack exposing
    ( FieldComponent
    , Msg(..)
    , Stack
    , add
    , init
    )

import Effect exposing (Effect)
import Form
import Form.Field exposing (Field)


{-| The Stack Msg type
-}
type Msg current previous
    = CurrentMsg current
    | PreviousMsg previous


{-| A Stack combines fields into a single TEA component
-}
type alias Stack customError output sharedMsg model msg =
    { init : Form.Form customError output -> ( model, Effect sharedMsg msg )
    , update : Form.Form customError output -> msg -> model -> ( model, Maybe Form.Msg, Effect sharedMsg msg )
    , subscriptions : Form.Form customError output -> model -> Sub msg
    }


type alias FieldComponent customError a model sharedMsg msg =
    { init : Form.FieldState customError a -> ( model, Effect sharedMsg msg )
    , update : Form.FieldState customError a -> msg -> model -> ( model, Maybe Form.Msg, Effect sharedMsg msg )
    , subscriptions : Form.FieldState customError a -> model -> Sub msg
    }


{-| Setup a new stack

The defaultView is used when no other view can be applied, which should never
happen if the application is properly defined.

-}
init : Stack customError output sharedMsg () ()
init =
    { init = \_ -> ( (), Effect.none )
    , update = \_ () () -> ( (), Nothing, Effect.none )
    , subscriptions = \_ () -> Sub.none
    }


{-| Add a page to a Stack
-}
add :
    String
    -> (Field -> Maybe a)
    -> FieldComponent customError a fieldModel sharedMsg fieldMsg
    -> Stack customError output sharedMsg previousModel previousMsg
    -> Stack customError output sharedMsg ( fieldModel, previousModel ) (Msg fieldMsg previousMsg)
add path fromField field previousStack =
    { init =
        \form ->
            let
                fieldstate =
                    Form.getFieldAs fromField path form

                ( fieldModel, fieldEffect ) =
                    field.init fieldstate

                ( previousModel, previousEffect ) =
                    previousStack.init form
            in
            ( ( fieldModel, previousModel )
            , Effect.batch
                [ Effect.map PreviousMsg previousEffect
                , Effect.map CurrentMsg fieldEffect
                ]
            )
    , update =
        \form msg ( fieldModel, previousModel ) ->
            case msg of
                CurrentMsg fieldMsg ->
                    let
                        fieldstate =
                            Form.getFieldAs fromField path form

                        ( newFieldModel, formMsg, fieldEffect ) =
                            field.update fieldstate fieldMsg fieldModel
                    in
                    ( ( newFieldModel, previousModel )
                    , formMsg
                    , Effect.map CurrentMsg fieldEffect
                    )

                PreviousMsg previousMsg ->
                    let
                        ( newPreviousModel, formMsg, previousEffect ) =
                            previousStack.update form previousMsg previousModel
                    in
                    ( ( fieldModel, newPreviousModel )
                    , formMsg
                    , Effect.map PreviousMsg previousEffect
                    )
    , subscriptions =
        \form ( fieldModel, previousModel ) ->
            let
                fieldstate =
                    Form.getFieldAs fromField path form
            in
            Sub.batch
                [ Sub.map CurrentMsg <| field.subscriptions fieldstate fieldModel
                , Sub.map PreviousMsg <| previousStack.subscriptions form previousModel
                ]
    }
