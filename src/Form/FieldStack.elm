module Form.FieldStack exposing
    ( FieldComponent
    , Msg(..)
    , Stack
    , add
    , addStack
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
    { init : String -> Form.Form customError output -> ( model, Effect sharedMsg msg )
    , update : String -> Form.Form customError output -> msg -> model -> ( model, Maybe Form.Msg, Effect sharedMsg msg )
    , subscriptions : String -> Form.Form customError output -> model -> Sub msg
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
    { init = \_ _ -> ( (), Effect.none )
    , update = \_ _ () () -> ( (), Nothing, Effect.none )
    , subscriptions = \_ _ () -> Sub.none
    }


path : String -> String -> String
path prefix name =
    if prefix == "" then
        name

    else
        prefix ++ "." ++ name


{-| Add a page to a Stack
-}
add :
    String
    -> (Field -> Maybe a)
    -> FieldComponent customError a fieldModel sharedMsg fieldMsg
    -> Stack customError output sharedMsg previousModel previousMsg
    -> Stack customError output sharedMsg ( fieldModel, previousModel ) (Msg fieldMsg previousMsg)
add name fromField field previousStack =
    { init =
        \prefix form ->
            let
                fieldstate =
                    Form.getFieldAs fromField (path prefix name) form

                ( fieldModel, fieldEffect ) =
                    field.init fieldstate

                ( previousModel, previousEffect ) =
                    previousStack.init prefix form
            in
            ( ( fieldModel, previousModel )
            , Effect.batch
                [ Effect.map PreviousMsg previousEffect
                , Effect.map CurrentMsg fieldEffect
                ]
            )
    , update =
        \prefix form msg ( fieldModel, previousModel ) ->
            case msg of
                CurrentMsg fieldMsg ->
                    let
                        fieldstate =
                            Form.getFieldAs fromField (path prefix name) form

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
                            previousStack.update prefix form previousMsg previousModel
                    in
                    ( ( fieldModel, newPreviousModel )
                    , formMsg
                    , Effect.map PreviousMsg previousEffect
                    )
    , subscriptions =
        \prefix form ( fieldModel, previousModel ) ->
            let
                fieldstate =
                    Form.getFieldAs fromField (path prefix name) form
            in
            Sub.batch
                [ Sub.map CurrentMsg <| field.subscriptions fieldstate fieldModel
                , Sub.map PreviousMsg <| previousStack.subscriptions prefix form previousModel
                ]
    }


addStack :
    String
    -> Stack customError output sharedMsg groupModel groupMsg
    -> Stack customError output sharedMsg previousModel previousMsg
    -> Stack customError output sharedMsg ( groupModel, previousModel ) (Msg groupMsg previousMsg)
addStack name group previousStack =
    { init =
        \prefix form ->
            let
                ( previousModel, previousEffect ) =
                    previousStack.init prefix form

                ( groupModel, groupEffect ) =
                    group.init (path prefix name) form
            in
            ( ( groupModel, previousModel )
            , Effect.batch
                [ Effect.map PreviousMsg previousEffect
                , Effect.map CurrentMsg groupEffect
                ]
            )
    , update =
        \prefix form msg ( groupModel, previousModel ) ->
            case msg of
                CurrentMsg groupMsg ->
                    let
                        ( newGroupModel, formMsg, groupEffect ) =
                            group.update (path prefix name) form groupMsg groupModel
                    in
                    ( ( newGroupModel, previousModel )
                    , formMsg
                    , Effect.map CurrentMsg groupEffect
                    )

                PreviousMsg previousMsg ->
                    let
                        ( newPreviousModel, formMsg, previousEffect ) =
                            previousStack.update prefix form previousMsg previousModel
                    in
                    ( ( groupModel, newPreviousModel )
                    , formMsg
                    , Effect.map PreviousMsg previousEffect
                    )
    , subscriptions =
        \prefix form ( groupModel, previousModel ) ->
            Sub.batch
                [ Sub.map CurrentMsg <| group.subscriptions (path prefix name) form groupModel
                , Sub.map PreviousMsg <| previousStack.subscriptions prefix form previousModel
                ]
    }
