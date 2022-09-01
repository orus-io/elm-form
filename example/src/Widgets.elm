module Widgets exposing (..)

import Element exposing (Element)
import Element.Input as Input
import Form
import Form.Field


textInput : Form.FieldState customError String -> Element Form.Msg
textInput { path, value } =
    Input.text []
        { onChange = Form.Field.String >> Form.Input path Form.Text
        , text = value |> Maybe.withDefault ""
        , label = Input.labelHidden ""
        , placeholder = Nothing
        }
