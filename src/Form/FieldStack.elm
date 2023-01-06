module Form.FieldStack exposing
    ( FieldComponent
    , Msg(..)
    , Stack
    , add
    , addList
    , addStack
    , addStackList
    , init
    , itempath
    , path
    )

import Effect exposing (Effect)
import Form
import Form.Field exposing (Field)
import List.Extra as List


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


itempath : String -> String -> Int -> String
itempath prefix name index =
    (if prefix == "" then
        name

     else
        prefix ++ "." ++ name
    )
        ++ "."
        ++ String.fromInt index


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


addList :
    String
    -> (Field -> Maybe a)
    -> FieldComponent customError a fieldModel sharedMsg fieldMsg
    -> Stack customError output sharedMsg previousModel previousMsg
    -> Stack customError output sharedMsg ( List fieldModel, previousModel ) (Msg ( Int, fieldMsg ) previousMsg)
addList name fromField field previousStack =
    let
        adjustListSize : String -> Form.Form customError output -> List fieldModel -> ( List fieldModel, Effect sharedMsg (Msg ( Int, fieldMsg ) previousMsg) )
        adjustListSize prefix form groupList =
            Form.getListIndexes (path prefix name) form
                |> List.foldl
                    (\i ( sourceList, modelList, effectList ) ->
                        case sourceList of
                            [] ->
                                let
                                    fieldstate =
                                        Form.getFieldAs fromField (itempath prefix name i) form

                                    ( fieldModel, fieldEffect ) =
                                        field.init fieldstate
                                in
                                ( [], fieldModel :: modelList, Effect.map (Tuple.pair i) fieldEffect :: effectList )

                            head :: tail ->
                                ( tail, head :: modelList, effectList )
                    )
                    ( groupList, [], [] )
                |> (\( _, list, effect ) ->
                        ( List.reverse list, Effect.batch effect |> Effect.map CurrentMsg )
                   )
    in
    { init =
        \prefix form ->
            let
                ( previousModel, previousEffect ) =
                    previousStack.init prefix form

                ( fieldListModel, fieldListEffect ) =
                    adjustListSize prefix form []
            in
            ( ( fieldListModel, previousModel )
            , Effect.batch
                [ Effect.map PreviousMsg previousEffect
                , fieldListEffect
                ]
            )
    , update =
        \prefix form msg ( fieldListModel, previousModel ) ->
            case msg of
                CurrentMsg ( index, fieldMsg ) ->
                    let
                        ( adjustedList, adjustedListEffect ) =
                            adjustListSize prefix form fieldListModel

                        ( updatedList, updatedListEffect ) =
                            adjustedList
                                |> updateAtWithEffect index
                                    (\fieldModel ->
                                        let
                                            fieldstate =
                                                Form.getFieldAs fromField (itempath prefix name index) form

                                            ( newFieldModel, formMsg, fieldEffect ) =
                                                field.update fieldstate fieldMsg fieldModel
                                        in
                                        ( newFieldModel, ( formMsg, Effect.map (Tuple.pair index) fieldEffect ) )
                                    )
                    in
                    ( ( updatedList, previousModel )
                    , updatedListEffect |> Maybe.andThen Tuple.first
                    , Effect.batch
                        [ updatedListEffect
                            |> Maybe.map (Tuple.second >> Effect.map CurrentMsg)
                            |> Maybe.withDefault Effect.none
                        , adjustedListEffect
                        ]
                    )

                PreviousMsg previousMsg ->
                    let
                        ( newPreviousModel, formMsg, previousEffect ) =
                            previousStack.update prefix form previousMsg previousModel
                    in
                    ( ( fieldListModel, newPreviousModel )
                    , formMsg
                    , Effect.map PreviousMsg previousEffect
                    )
    , subscriptions =
        \prefix form ( fieldListModel, previousModel ) ->
            Sub.batch
                [ adjustListSize prefix form fieldListModel
                    |> Tuple.first
                    |> List.indexedMap
                        (\i fieldModel ->
                            let
                                fieldState =
                                    Form.getFieldAs fromField (itempath prefix name i) form
                            in
                            field.subscriptions fieldState fieldModel
                                |> Sub.map (Tuple.pair i)
                        )
                    |> Sub.batch
                    |> Sub.map CurrentMsg
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


updateAtWithEffect : Int -> (a -> ( a, effect )) -> List a -> ( List a, Maybe effect )
updateAtWithEffect index update =
    List.indexedFoldr
        (\i a ( r, e ) ->
            if i == index then
                let
                    ( newa, effect ) =
                        update a
                in
                ( newa :: r, Just effect )

            else
                ( a :: r, e )
        )
        ( [], Nothing )


addStackList :
    String
    -> Stack customError output sharedMsg groupModel groupMsg
    -> Stack customError output sharedMsg previousModel previousMsg
    -> Stack customError output sharedMsg ( List groupModel, previousModel ) (Msg ( Int, groupMsg ) previousMsg)
addStackList name group previousStack =
    let
        adjustListSize : String -> Form.Form customError output -> List groupModel -> ( List groupModel, Effect sharedMsg (Msg ( Int, groupMsg ) previousMsg) )
        adjustListSize prefix form groupList =
            Form.getListIndexes (path prefix name) form
                |> List.foldl
                    (\i ( sourceList, modelList, effectList ) ->
                        case sourceList of
                            [] ->
                                let
                                    ( groupModel, groupEffect ) =
                                        group.init (itempath prefix name i) form
                                in
                                ( [], groupModel :: modelList, Effect.map (Tuple.pair i) groupEffect :: effectList )

                            head :: tail ->
                                ( tail, head :: modelList, effectList )
                    )
                    ( groupList, [], [] )
                |> (\( _, list, effect ) ->
                        ( List.reverse list, Effect.batch effect |> Effect.map CurrentMsg )
                   )
    in
    { init =
        \prefix form ->
            let
                ( previousModel, previousEffect ) =
                    previousStack.init prefix form

                ( groupListModel, groupListEffect ) =
                    adjustListSize prefix form []
            in
            ( ( groupListModel, previousModel )
            , Effect.batch
                [ Effect.map PreviousMsg previousEffect
                , groupListEffect
                ]
            )
    , update =
        \prefix form msg ( groupListModel, previousModel ) ->
            case msg of
                CurrentMsg ( index, groupMsg ) ->
                    let
                        ( adjustedList, adjustedListEffect ) =
                            adjustListSize prefix form groupListModel

                        ( updatedList, updatedListEffect ) =
                            adjustedList
                                |> updateAtWithEffect index
                                    (\groupModel ->
                                        let
                                            ( newGroupModel, formMsg, groupEffect ) =
                                                group.update (itempath prefix name index)
                                                    form
                                                    groupMsg
                                                    groupModel
                                        in
                                        ( newGroupModel, ( formMsg, Effect.map (Tuple.pair index) groupEffect ) )
                                    )
                    in
                    ( ( updatedList, previousModel )
                    , updatedListEffect |> Maybe.andThen Tuple.first
                    , Effect.batch
                        [ updatedListEffect
                            |> Maybe.map (Tuple.second >> Effect.map CurrentMsg)
                            |> Maybe.withDefault Effect.none
                        , adjustedListEffect
                        ]
                    )

                PreviousMsg previousMsg ->
                    let
                        ( newPreviousModel, formMsg, previousEffect ) =
                            previousStack.update prefix form previousMsg previousModel
                    in
                    ( ( groupListModel, newPreviousModel )
                    , formMsg
                    , Effect.map PreviousMsg previousEffect
                    )
    , subscriptions =
        \prefix form ( groupListModel, previousModel ) ->
            Sub.batch
                [ groupListModel
                    |> List.indexedMap
                        (\i groupModel ->
                            group.subscriptions (itempath prefix name i) form groupModel
                                |> Sub.map (Tuple.pair i)
                        )
                    |> Sub.batch
                    |> Sub.map CurrentMsg
                , Sub.map PreviousMsg <| previousStack.subscriptions prefix form previousModel
                ]
    }
