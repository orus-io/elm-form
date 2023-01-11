module LoginForm exposing (..)

import Element
import Form
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type alias Credentials =
    { login : String
    , password : String
    , rememberMe : Bool
    }


type alias FormData =
    Form.Model String Credentials ()


type alias Msg =
    Form.Msg ()


form =
    Form.init
        { validate =
            \_ login password rememberMe ->
                Validate.succeed Credentials
                    |> Validate.andMap login.valid
                    |> Validate.andMap password.valid
                    |> Validate.andMap rememberMe.valid
        , view =
            \model login password rememberMe ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.textInput login
                    , Widgets.textInput password
                    , Widgets.checkbox rememberMe
                    ]
        }
        |> Form.field "login"
            (Field.text
                |> Field.withInitialValue .login
            )
        |> Form.field "password"
            (Field.text
                |> Field.withInitialValue .password
            )
        |> Form.field "remember-me" (Field.boolean |> Field.withInitialValue .rememberMe)
        |> Form.finalize
