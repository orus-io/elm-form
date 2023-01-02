module PersonForm exposing (..)

import Element exposing (Element)
import Element.Input as Input
import Form.Builder as Builder
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type alias Person =
    { firstname : String
    , lastname : String
    , phoneNumbers : List String
    }


viewPerson : Person -> Element msg
viewPerson person =
    Element.column [] <|
        (Element.text <| person.firstname ++ " " ++ person.lastname)
            :: List.map Element.text person.phoneNumbers


type alias FormData =
    Builder.Model String Person ()


type alias Msg =
    Builder.Msg ()


form =
    Builder.init
        { validate =
            \firstname lastname phoneNumbers _ ->
                Validate.succeed Person
                    |> Validate.andMap firstname.valid
                    |> Validate.andMap lastname.valid
                    |> Validate.andMap phoneNumbers.valid

        --|> Validate.andMap phoneNumbers.valid
        , view =
            \firstname lastname phoneNumbers _ ->
                Element.column
                    [ Element.spacing 20
                    ]
                    [ Widgets.textInput firstname
                    , Widgets.textInput lastname
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
        |> Builder.field "firstname" (Field.text |> Field.withInitialValue .firstname)
        |> Builder.field "lastname" (Field.text |> Field.withInitialValue .lastname)
        |> Builder.list "phone" (Just .phoneNumbers) Field.text
        |> Builder.finalize
