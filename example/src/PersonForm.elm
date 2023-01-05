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
    , addressList : List Address
    , phoneNumbers : List String
    }


viewAddress : Address -> Element msg
viewAddress { street, city, zip_code } =
    Element.text <| street ++ " " ++ city ++ " " ++ zip_code


viewPerson : Person -> Element msg
viewPerson person =
    Element.column [] <|
        (Element.text <| person.firstname ++ " " ++ person.lastname)
            :: List.concat
                [ List.map Element.text person.phoneNumbers
                , List.map viewAddress person.addressList
                ]


type alias StackMsg =
    Form.FieldStack.Msg ( Int, AddressForm.StackMsg ) ()


type alias Msg =
    Builder.Msg StackMsg


type alias FormData =
    Builder.Model
        String
        Person
        ( List ( Dropdown.State AddressForm.StreetKind, () ), () )


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
                    , Input.button [] { label = Element.text "Add Address", onPress = Just address.onAppend }
                    , address.items |> Element.column []
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
        |> Builder.groupList "addressList" .addressList AddressForm.group
        |> Builder.list "phone" (Just .phoneNumbers) Field.text
        |> Builder.finalize
