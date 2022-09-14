module Widgets exposing (..)

import Dropdown
import Effect
import Element exposing (Attribute, Element)
import Element.Events as Events
import Element.Input as Input
import Form
import Form.Builder as Builder
import Form.Field
import Form.FieldStack as FieldStack


textInput : Builder.FieldViewState customError String stackMsg -> Element (Builder.Msg stackMsg)
textInput { state, onInput, onFocus, onBlur } =
    Input.text
        [ Events.onFocus onFocus
        , Events.onLoseFocus onBlur
        ]
        { onChange = onInput Form.Text
        , text = state.value |> Maybe.withDefault ""
        , label = Input.labelHidden ""
        , placeholder = Nothing
        }


checkbox : Builder.FieldViewState customError Bool stackMsg -> Element (Builder.Msg stackMsg)
checkbox { state, onFocus, onBlur, onInput } =
    Input.checkbox
        [ Events.onFocus onFocus
        , Events.onLoseFocus onBlur
        ]
        { onChange = onInput Form.Checkbox
        , icon = Input.defaultCheckbox
        , label = Input.labelHidden ""
        , checked = state.value |> Maybe.withDefault False
        }


radio :
    List (Attribute (Builder.Msg stackMsg))
    ->
        { options : List (Input.Option value (Builder.Msg stackMsg))
        , label : Input.Label (Builder.Msg stackMsg)
        }
    -> Builder.FieldViewState customError value stackMsg
    -> Element (Builder.Msg stackMsg)
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
                        |> Maybe.map (Builder.onInput fielddef Form.Select fieldstate)
                        |> Maybe.withDefault (Builder.onEmpty Form.Select fieldstate)
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
    , itemToPrompt : item -> Element (Builder.Msg stackMsg)
    , itemToElement : Bool -> Bool -> item -> Element (Builder.Msg stackMsg)
    , dropdownOptions :
        List
            (Dropdown.Config item (Builder.Msg stackMsg) ( Maybe item, List item )
             -> Dropdown.Config item (Builder.Msg stackMsg) ( Maybe item, List item )
            )
    }
    -> Builder.FieldComponentViewState customError item stackMsg (Dropdown.State item) (DropdownMsg item)
    -> Element (Builder.Msg stackMsg)
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
