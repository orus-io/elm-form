module Main exposing (..)

-- import AddressForm

import AddressForm
import Browser
import Effect
import Element
import Form
import LoginForm
import PersonForm


type alias Model =
    { loginForm : LoginForm.FormData
    , addressForm : AddressForm.FormData
    , personForm : PersonForm.FormData
    }


type Msg
    = Noop
    | LoginFormMsg LoginForm.Msg
    | AddressFormMsg AddressForm.Msg
    | PersonFormMsg PersonForm.Msg


init : () -> ( Model, Cmd Msg )
init flags =
    let
        ( loginForm, loginEffect ) =
            LoginForm.form.init () (Just { login = "login", password = "pass", rememberMe = False })

        ( addressForm, addressEffect ) =
            AddressForm.form.init () Nothing

        ( personForm, personEffect ) =
            PersonForm.form.init () Nothing
    in
    ( { loginForm = loginForm
      , addressForm = addressForm
      , personForm = personForm
      }
    , Effect.toCmd ( always Noop, identity ) <|
        Effect.batch
            [ Effect.map LoginFormMsg loginEffect
            , Effect.map AddressFormMsg addressEffect
            , Effect.map PersonFormMsg personEffect
            ]
    )


subscriptions model =
    Sub.map LoginFormMsg <| LoginForm.form.subscriptions () model.loginForm


update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        AddressFormMsg subMsg ->
            let
                ( addressForm, effect ) =
                    AddressForm.form.update () subMsg model.addressForm
            in
            ( { model | addressForm = addressForm }
            , Effect.toCmd ( always Noop, AddressFormMsg ) effect
            )

        LoginFormMsg subMsg ->
            let
                ( loginForm, effect ) =
                    LoginForm.form.update () subMsg model.loginForm
            in
            ( { model | loginForm = loginForm }
            , Effect.toCmd ( always Noop, LoginFormMsg ) effect
            )

        PersonFormMsg subMsg ->
            let
                ( personForm, effect ) =
                    PersonForm.form.update () subMsg model.personForm
            in
            ( { model | personForm = personForm }
            , Effect.toCmd ( always Noop, PersonFormMsg ) effect
            )


view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.el [ Element.width Element.fill ] <|
                Element.map LoginFormMsg <|
                    LoginForm.form.view () model.loginForm
            , Element.el [ Element.width Element.fill ] <|
                Element.map AddressFormMsg <|
                    AddressForm.form.view () model.addressForm
            , Element.column [ Element.width Element.fill, Element.spacing 20 ]
                [ Form.getOutput model.personForm.form
                    |> Maybe.map PersonForm.viewPerson
                    |> Maybe.withDefault (Element.text "invalid !")
                , Element.map PersonFormMsg <|
                    PersonForm.form.view () model.personForm
                ]
            ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
