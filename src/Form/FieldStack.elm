module Spa.FieldStack exposing
    ( Stack, setup, add
    , Msg, Model, empty, getError, routeChange
    , FieldSetup, CurrentViewMap, PreviousViewMap
    )

{-| This module provides the tools to combine multiple pages into a single TEA
component.

It can be used separately from Spa, in case it doesn't handle the complexity of
your application (and if it's the case I am interested to know about it!).

Another use case is to progressively port a hand-written application to Spa, by
embedding a FieldStack in the existing application, then port pages to it one by
one. Once all the pages are in the stack, the main application can be ported to
Spa.

@docs Stack, setup, add

@docs Msg, Model, empty, getError, routeChange

@docs FieldSetup, RouteMatcher, CurrentViewMap, PreviousViewMap

-}

import Effect exposing (Effect)


{-| The Stack Msg type
-}
type Msg route current previous
    = CurrentMsg current
    | PreviousMsg previous


{-| A Stack combines pages into a single TEA component
-}
type alias Stack customError a shared sharedMsg route view current previous currentMsg previousMsg =
    { init : shared -> route -> ( Model setupError current previous, Effect sharedMsg (Msg route currentMsg previousMsg) )
    , update : shared -> Msg route currentMsg previousMsg -> Model setupError current previous -> ( Model setupError current previous, Effect sharedMsg (Msg route currentMsg previousMsg) )
    , subscriptions : shared -> Model setupError current previous -> Sub (Msg route currentMsg previousMsg)
    }


{-| An empty model for initialising a stack state
-}
empty : Model a b
empty =
    ()


{-| returns the current setup error if any
-}
getError : Model setupError current previous -> Maybe setupError
getError model =
    case model of
        SetupError err ->
            Just err

        _ ->
            Nothing


mapPrevious : Model setupError pa pb -> Model setupError a (Model setupError pa pb)
mapPrevious m =
    case m of
        NoField ->
            NoField

        SetupError err ->
            SetupError err

        _ ->
            Previous m


{-| Setup a new stack

The defaultView is used when no other view can be applied, which should never
happen if the application is properly defined.

-}
setup :
    { defaultView : view }
    -> Stack setupError shared sharedMsg route view () () () ()
setup { defaultView } =
    { init = \_ _ -> ( NoField, Effect.none )
    , update = \_ _ _ -> ( NoField, Effect.none )
    , view = \_ _ -> defaultView
    , subscriptions = \_ _ -> Sub.none
    }


{-| A view mapper, for example Html.map or Element.map depending on your actual
view type.
-}
type alias CurrentViewMap route currentMsg previousMsg pageView view =
    (currentMsg -> Msg route currentMsg previousMsg) -> pageView -> view


{-| A view mapper, for example Html.map or Element.map depending on your actual
view type.
-}
type alias PreviousViewMap route currentMsg previousMsg previousView view =
    (previousMsg -> Msg route currentMsg previousMsg) -> previousView -> view


{-| Add a page to a Stack
-}
add :
    ( CurrentViewMap route currentMsg previousMsg pageView view
    , PreviousViewMap route currentMsg previousMsg previousView view
    )
    -> RouteMatcher route flags
    -> FieldSetup setupError flags shared sharedMsg pageView pageModel pageMsg
    -> Stack setupError shared sharedMsg route previousView previousCurrent previousPrevious previousCurrentMsg previousPreviousMsg
    -> Stack setupError shared sharedMsg route view pageModel (Model setupError previousCurrent previousPrevious) pageMsg (Msg route previousCurrentMsg previousPreviousMsg)
add ( mapFieldView, mapPreviousView ) match pagesetup previousStack =
    { init =
        \shared route ->
            case match route of
                Just flags ->
                    case pagesetup shared of
                        Ok (Internal.Field page) ->
                            let
                                ( pageModel, pageEffect ) =
                                    page.init flags
                            in
                            ( Current pageModel
                            , Effect.map CurrentMsg pageEffect
                            )

                        Err err ->
                            ( SetupError err, Effect.none )

                Nothing ->
                    let
                        ( prevModel, prevEffect ) =
                            previousStack.init shared route
                    in
                    ( mapPrevious prevModel
                    , Effect.map PreviousMsg prevEffect
                    )
    , update =
        \shared msg model ->
            case ( msg, model ) of
                ( RouteChange route, _ ) ->
                    case match route of
                        Just flags ->
                            case pagesetup shared of
                                Ok (Internal.Field page) ->
                                    case ( page.onNewFlags, model ) of
                                        ( Just tomsg, Current pageModel ) ->
                                            -- we already are on the right page, and it has
                                            -- a 'onNewFlags' message
                                            let
                                                ( newFieldModel, pageEffect ) =
                                                    page.update (tomsg flags) pageModel
                                            in
                                            ( Current newFieldModel
                                            , Effect.map CurrentMsg pageEffect
                                            )

                                        _ ->
                                            let
                                                ( pageModel, pageEffect ) =
                                                    page.init flags
                                            in
                                            ( Current pageModel
                                            , Effect.map CurrentMsg pageEffect
                                            )

                                Err err ->
                                    ( SetupError err, Effect.none )

                        Nothing ->
                            -- current page doesn't match, let lower layers find the new one
                            let
                                ( newPreviousModel, previousEffect ) =
                                    previousStack.update shared (RouteChange route) (getPrevious model)
                            in
                            ( mapPrevious newPreviousModel, Effect.map PreviousMsg previousEffect )

                ( CurrentMsg pageMsg, Current pageModel ) ->
                    case pagesetup shared of
                        Ok (Internal.Field page) ->
                            let
                                ( newFieldModel, newFieldEffect ) =
                                    page.update pageMsg pageModel
                            in
                            ( Current newFieldModel, Effect.map CurrentMsg newFieldEffect )

                        Err err ->
                            ( SetupError err, Effect.none )

                ( PreviousMsg previousMsg, Previous previousModel ) ->
                    let
                        ( newPreviousModel, previousEffect ) =
                            previousStack.update shared previousMsg previousModel
                    in
                    ( mapPrevious newPreviousModel, Effect.map PreviousMsg previousEffect )

                ( _, NoField ) ->
                    ( model, Effect.none )

                ( CurrentMsg _, _ ) ->
                    ( model, Effect.none )

                ( PreviousMsg _, _ ) ->
                    ( model, Effect.none )
    , subscriptions =
        \shared model ->
            case model of
                NoField ->
                    Sub.none

                SetupError _ ->
                    Sub.none

                Current pageModel ->
                    case pagesetup shared of
                        Ok (Internal.Field page) ->
                            page.subscriptions pageModel
                                |> Sub.map CurrentMsg

                        Err _ ->
                            Sub.none

                Previous prevModel ->
                    previousStack.subscriptions shared prevModel
                        |> Sub.map PreviousMsg
    , view =
        \shared model ->
            case model of
                Current pageModel ->
                    case pagesetup shared of
                        Ok (Internal.Field page) ->
                            page.view pageModel
                                |> mapFieldView CurrentMsg

                        Err _ ->
                            previousStack.view shared NoField
                                |> mapPreviousView PreviousMsg

                Previous previousModel ->
                    previousStack.view shared previousModel
                        |> mapPreviousView PreviousMsg

                NoField ->
                    previousStack.view shared NoField
                        |> mapPreviousView PreviousMsg

                SetupError _ ->
                    previousStack.view shared NoField
                        |> mapPreviousView PreviousMsg
    }
