module LoginForm exposing (..)

import Element
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type alias Credentials =
    { login : String
    , password : String
    , rememberMe : Bool
    }


type alias Model =
    { model : ()
    , data : Form String Credentials
    }


type alias FormData =
    Form String Credentials


type alias Msg =
    Form.Msg


form =
    Form.init
        { validate =
            \login password rememberMe _ ->
                Validate.succeed Credentials
                    |> Validate.andMap (Validate.field login Validate.string)
                    |> Validate.andMap (Validate.field password Validate.string)
                    |> Validate.andMap (Validate.field rememberMe Validate.bool)
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
        |> Form.field "login"
            (Field.text
                |> Field.withInitialValue .login
            )
        |> Form.field "password"
            (Field.text
                |> Field.withInitialValue .password
            )
        |> Form.field "remember-me" Field.boolean
        |> Form.finalize