module Main exposing (..)

-- import AddressForm

import Browser
import Effect
import Element
import LoginForm


type alias Model =
    { loginForm : LoginForm.FormData }


type Msg
    = Noop
    | LoginFormMsg LoginForm.Msg


init : () -> ( Model, Cmd Msg )
init flags =
    ( { loginForm = LoginForm.form.init () Nothing }, Cmd.none )


subscriptions model =
    Sub.map LoginFormMsg <| LoginForm.form.subscriptions () model.loginForm


update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        LoginFormMsg subMsg ->
            let
                ( loginForm, effect ) =
                    LoginForm.form.update () subMsg model.loginForm
            in
            ( { model | loginForm = loginForm }
            , Effect.toCmd ( always Noop, LoginFormMsg ) effect
            )


view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.map LoginFormMsg <|
            LoginForm.form.view () model.loginForm


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
