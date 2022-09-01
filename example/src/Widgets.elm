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


checkbox : Form.FieldState customError Bool -> Element Form.Msg
checkbox { path, value } =
    Input.checkbox []
        { onChange = Form.Field.Bool >> Form.Input path Form.Checkbox
        , icon = Input.defaultCheckbox
        , label = Input.labelHidden ""
        , checked = value |> Maybe.withDefault False
        }
