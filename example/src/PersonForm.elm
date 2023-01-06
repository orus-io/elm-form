module PersonForm exposing (..)

import AddressForm exposing (Address)
import Dropdown
import Element exposing (Element)
import Element.Input as Input
import Enum exposing (Enum)
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
    , role : List Role
    }


type Role
    = RoleCEO
    | RoleCTO
    | RoleManager
    | RoleEmployee


roleEnum : Enum Role
roleEnum =
    Enum.enum
        [ ( RoleCEO, "ceo" )
        , ( RoleCTO, "cto" )
        , ( RoleManager, "manager" )
        , ( RoleEmployee, "employee" )
        ]


roleLabel : Role -> String
roleLabel role =
    case role of
        RoleCEO ->
            "CEO"

        RoleCTO ->
            "CTO"

        RoleManager ->
            "Manager"

        RoleEmployee ->
            "Employee"


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
    Form.FieldStack.Msg
        ( Int, Widgets.DropdownMsg Role )
        (Form.FieldStack.Msg ( Int, AddressForm.StackMsg ) ())


type alias Msg =
    Builder.Msg StackMsg


type alias FormData =
    Builder.Model
        String
        Person
        ( List (Dropdown.State Role)
        , ( List ( Dropdown.State AddressForm.StreetKind, () ), () )
        )


form =
    Builder.init
        { validate =
            \_ firstname lastname address phoneNumbers roleList ->
                Validate.succeed Person
                    |> Validate.andMap firstname.valid
                    |> Validate.andMap lastname.valid
                    |> Validate.andMap address
                    |> Validate.andMap phoneNumbers.valid
                    |> Validate.andMap roleList.valid
        , view =
            \_ firstname lastname address phoneNumbers roleList ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.textInput firstname
                    , Widgets.textInput lastname
                    , Input.button [] { label = Element.text "add address", onPress = Just address.onAppend }
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
                    , Input.button [] { onPress = Just phoneNumbers.onAppend, label = Element.text "add phone number" }
                    , let
                        usedOptions =
                            roleList.items
                                |> List.filterMap (.viewstate >> .state >> .value)

                        lines =
                            roleList.items
                                |> List.map
                                    (\item ->
                                        Element.row [ Element.spacing 20 ]
                                            [ Widgets.dropdownSelectView
                                                { options =
                                                    roleEnum.values
                                                        |> List.filter
                                                            (\v ->
                                                                not (List.member v usedOptions)
                                                                    || (Just v == item.viewstate.state.value)
                                                            )
                                                , dropdownOptions = []
                                                , itemToPrompt = roleLabel >> Element.text
                                                , itemToElement = \_ _ -> roleLabel >> Element.text
                                                }
                                                item.viewstate
                                            , Input.button [] { onPress = Just item.onRemove, label = Element.text "X" }
                                            ]
                                    )
                      in
                      Element.column [] lines
                    , if List.length roleList.items < List.length roleEnum.values then
                        Input.button [] { onPress = Just roleList.onAppend, label = Element.text "add role" }

                      else
                        Element.none
                    ]
        }
        |> Builder.field "firstname" Field.text
        |> Builder.field "lastname" Field.text
        |> Builder.groupList "addressList" .addressList AddressForm.group
        |> Builder.list "phone" (Just .phoneNumbers) Field.text
        |> Builder.listWithState "role" (Just .role) (Field.custom roleEnum.toString roleEnum.fromString) Widgets.dropdownSelect
        |> Builder.finalize
