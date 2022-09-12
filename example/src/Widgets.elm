module Widgets exposing (..)

import Element exposing (Attribute, Element)
import Element.Events as Events
import Element.Input as Input
import Form
import Form.Field


textInput : Form.FieldViewState customError String -> Element Form.Msg
textInput { state } =
    Input.text
        [ Events.onFocus <| Form.Focus state.path
        , Events.onLoseFocus <| Form.Blur state.path
        ]
        { onChange = Form.Field.String >> Form.Input state.path Form.Text
        , text = state.value |> Maybe.withDefault ""
        , label = Input.labelHidden ""
        , placeholder = Nothing
        }


checkbox : Form.FieldViewState customError Bool -> Element Form.Msg
checkbox { state } =
    Input.checkbox
        [ Events.onFocus <| Form.Focus state.path
        , Events.onLoseFocus <| Form.Blur state.path
        ]
        { onChange = Form.Field.Bool >> Form.Input state.path Form.Checkbox
        , icon = Input.defaultCheckbox
        , label = Input.labelHidden ""
        , checked = state.value |> Maybe.withDefault False
        }


radio :
    List (Attribute Form.Msg)
    ->
        { options : List (Input.Option value Form.Msg)
        , label : Input.Label Form.Msg
        }
    -> Form.FieldViewState customError value
    -> Element Form.Msg
radio attrs { options, label } { state, onFocus, onBlur, onInput } =
    Input.radio
        (Events.onFocus onFocus
            :: Events.onLoseFocus onBlur
            :: attrs
        )
        { label = label
        , onChange = onInput Form.Radio
        , options = options
        , selected = state.value
        }
