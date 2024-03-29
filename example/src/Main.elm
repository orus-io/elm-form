module Main exposing (..)

-- import PersonForm

import Browser
import Effect
import Element
import LoginForm
import PersonForm


type alias Model =
    { loginForm : LoginForm.FormData
    , personForm : PersonForm.FormData
    }


type Msg
    = Noop
    | LoginFormMsg LoginForm.Msg
    | PersonFormMsg PersonForm.Msg


init : () -> ( Model, Cmd Msg )
init flags =
    let
        ( loginForm, loginEffect ) =
            LoginForm.form.init () (Just { login = "login", password = "pass", rememberMe = False })

        ( personForm, personEffect ) =
            PersonForm.form.init () Nothing
    in
    ( { loginForm = loginForm
      , personForm = personForm
      }
    , Effect.toCmd ( always Noop, identity ) <|
        Effect.batch
            [ Effect.map LoginFormMsg loginEffect
            , Effect.map PersonFormMsg personEffect
            ]
    )


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
            , Element.column [ Element.width Element.fill, Element.spacing 20 ]
                [ model.personForm.output
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
