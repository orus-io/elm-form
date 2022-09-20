module PersonForm exposing (..)

import AddressForm exposing (Address)
import Dropdown
import Element exposing (Element)
import Element.Input as Input
import Form.Builder as Builder
import Form.Field as Field
import Form.FieldStack
import Form.Validate as Validate
import Widgets


type alias Person =
    { firstname : String
    , lastname : String
    , address : Address
    , phoneNumbers : List String
    }


viewPerson : Person -> Element msg
viewPerson person =
    Element.column [] <|
        (Element.text <| person.firstname ++ " " ++ person.lastname)
            :: List.map Element.text person.phoneNumbers


type alias Msg =
    Builder.Msg
        (Form.FieldStack.Msg
            (Form.FieldStack.Msg
                (Widgets.DropdownMsg AddressForm.StreetKind)
                ()
            )
            ()
        )


type alias FormData =
    Builder.Model
        String
        Person
        ( ( Dropdown.State AddressForm.StreetKind, () ), () )


form =
    Builder.init
        { validate =
            \_ firstname lastname address phoneNumbers ->
                Validate.succeed Person
                    |> Validate.andMap firstname.valid
                    |> Validate.andMap lastname.valid
                    |> Validate.andMap address
                    |> Validate.andMap phoneNumbers.valid
        , view =
            \_ firstname lastname address phoneNumbers ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.textInput firstname
                    , Widgets.textInput lastname
                    , address
                    , List.map
                        (\item ->
                            Element.row []
                                [ Widgets.textInput item.viewstate
                                , Input.button [] { onPress = Just item.onRemove, label = Element.text "X" }
                                ]
                        )
                        phoneNumbers.items
                        |> Element.column []
                    , Input.button [] { onPress = Just phoneNumbers.onAppend, label = Element.text "nouveau numéro" }
                    ]
        }
        |> Builder.field "firstname" Field.text
        |> Builder.field "lastname" Field.text
        |> Builder.group "address" .address AddressForm.group
        |> Builder.list "phone" (Just .phoneNumbers) Field.text
        |> Builder.finalize
