module Widgets exposing (..)

import Dropdown
import Effect
import Element exposing (Attribute, Element)
import Element.Events as Events
import Element.Input as Input
import Form.InputType as InputType
import Form
import Form.Field
import Form.FieldStack as FieldStack


textInput : Form.FieldViewState customError String stackMsg -> Element (Form.Msg stackMsg)
textInput { state, onInput, onFocus, onBlur } =
    Input.text
        [ Events.onFocus onFocus
        , Events.onLoseFocus onBlur
        ]
        { onChange = onInput InputType.Text
        , text = state.value |> Maybe.withDefault ""
        , label = Input.labelHidden ""
        , placeholder = Nothing
        }


checkbox : Form.FieldViewState customError Bool stackMsg -> Element (Form.Msg stackMsg)
checkbox { state, onFocus, onBlur, onInput } =
    Input.checkbox
        [ Events.onFocus onFocus
        , Events.onLoseFocus onBlur
        ]
        { onChange = onInput InputType.Checkbox
        , icon = Input.defaultCheckbox
        , label = Input.labelHidden ""
        , checked = state.value |> Maybe.withDefault False
        }


radio :
    List (Attribute (Form.Msg stackMsg))
    ->
        { options : List (Input.Option value (Form.Msg stackMsg))
        , label : Input.Label (Form.Msg stackMsg)
        }
    -> Form.FieldViewState customError value stackMsg
    -> Element (Form.Msg stackMsg)
radio attrs { options, label } { state, onFocus, onBlur, onInput } =
    Input.radio
        (Events.onFocus onFocus
            :: Events.onLoseFocus onBlur
            :: attrs
        )
        { label = label
        , onChange = onInput InputType.Radio
        , options = options
        , selected = state.value
        }


type DropdownMsg item
    = Noop
    | DropdownMsg (Dropdown.Msg item)
    | OnSelect (Maybe item)


dropdownSelect : Form.Field.FieldDef output item -> FieldStack.FieldComponent customError item (Dropdown.State item) sharedMsg (DropdownMsg item)
dropdownSelect fielddef =
    let
        config =
            Dropdown.basic
                { itemsFromModel = always []
                , selectionFromModel = .value
                , dropdownMsg = DropdownMsg
                , onSelectMsg = OnSelect
                , itemToPrompt = \_ -> Element.none
                , itemToElement = \_ _ _ -> Element.none
                }
    in
    { init = \fieldstate -> ( Dropdown.init fieldstate.path, Effect.none )
    , subscriptions = \_ state -> Dropdown.onOutsideClick state identity |> Sub.map DropdownMsg
    , update =
        \fieldstate msg state ->
            case msg of
                Noop ->
                    ( state, Nothing, Effect.none )

                OnSelect item ->
                    ( state
                    , item
                        |> Maybe.map (Form.onInput fielddef InputType.Select fieldstate)
                        |> Maybe.withDefault (Form.onEmpty InputType.Select fieldstate)
                        |> Just
                    , Effect.none
                    )

                DropdownMsg ddMsg ->
                    let
                        ( newState, cmd ) =
                            Dropdown.update config ddMsg fieldstate state
                    in
                    ( newState, Nothing, Effect.fromCmd cmd )
    }


dropdownSelectView :
    { options : List item
    , itemToPrompt : item -> Element (Form.Msg stackMsg)
    , itemToElement : Bool -> Bool -> item -> Element (Form.Msg stackMsg)
    , dropdownOptions :
        List
            (Dropdown.Config item (Form.Msg stackMsg) ( Maybe item, List item )
             -> Dropdown.Config item (Form.Msg stackMsg) ( Maybe item, List item )
            )
    }
    -> Form.FieldComponentViewState customError item stackMsg (Dropdown.State item) (DropdownMsg item)
    -> Element (Form.Msg stackMsg)
dropdownSelectView { options, itemToPrompt, itemToElement, dropdownOptions } { state, model, onFocus, onBlur, onInput, onEmpty, toMsg } =
    let
        config =
            List.foldl (\alterConfig -> alterConfig)
                (Dropdown.basic
                    { itemsFromModel = Tuple.second
                    , selectionFromModel = Tuple.first
                    , dropdownMsg = DropdownMsg >> toMsg
                    , onSelectMsg = OnSelect >> toMsg
                    , itemToPrompt = itemToPrompt
                    , itemToElement = itemToElement
                    }
                )
                dropdownOptions
    in
    Dropdown.view config ( state.value, options ) model
