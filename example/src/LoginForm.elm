module LoginForm exposing (..)

import Element
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type alias Credentials =
    { login : String
    , password : String
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
            \login password _ ->
                Validate.succeed Credentials
                    |> Validate.andMap (Validate.field login Validate.string)
                    |> Validate.andMap (Validate.field password Validate.string)
        , view =
            \login password model ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.textInput login
                    , Widgets.textInput password
                    ]
        }
        |> Form.field "login"
        |> Form.field "password"
        |> Form.finalize
