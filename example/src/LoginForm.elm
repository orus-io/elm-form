module LoginForm exposing (..)

import Element
import Form exposing (Form)
import Form.Builder as Builder
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type alias Credentials =
    { login : String
    , password : String
    , rememberMe : Bool
    }


type alias FormData =
    Builder.Model String Credentials ()


type alias Msg =
    Builder.Msg ()


form =
    Builder.init
        { validate =
            \login password rememberMe _ ->
                Validate.succeed Credentials
                    |> Validate.andMap login.valid
                    |> Validate.andMap password.valid
                    |> Validate.andMap rememberMe.valid
        , view =
            \login password rememberMe model ->
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
        |> Builder.field "login"
            (Field.text
                |> Field.withInitialValue .login
            )
        |> Builder.field "password"
            (Field.text
                |> Field.withInitialValue .password
            )
        |> Builder.field "remember-me" Field.boolean
        |> Builder.finalize
