module AddressForm exposing (..)

import Dropdown
import Element exposing (Element)
import Element.Input as Input
import Enum exposing (Enum)
import Form
import Form.Field as Field
import Form.FieldStack
import Form.Validate as Validate exposing (Validation)
import Widgets


type StreetKind
    = Avenue
    | Boulevard
    | Street


type alias FormData =
    Form.Model String Address ( Dropdown.State StreetKind, () )


type alias StackMsg =
    Form.FieldStack.Msg (Widgets.DropdownMsg StreetKind) ()


type alias Msg =
    Form.Msg StackMsg


streetKindEnum : Enum StreetKind
streetKindEnum =
    Enum.enum
        [ ( Avenue, "ave" )
        , ( Boulevard, "bld" )
        , ( Street, "str" )
        ]


streetKindLabel : StreetKind -> String
streetKindLabel kind =
    case kind of
        Avenue ->
            "Avenue"

        Boulevard ->
            "Boulevard"

        Street ->
            "Street"


type alias Address =
    { streetKind : StreetKind
    , street : String
    , zip_code : String
    , city : String
    }


group :
    Form.Builder
        (model -> Validation customError Address)
        (Element (Form.Msg topStackMsg))
        model
        customError
        sharedMsg
        data
        ( Dropdown.State StreetKind, () )
        StackMsg
        topStackMsg
group =
    Form.init
        { validate =
            \_ street_kind street zip_code city ->
                Validate.succeed Address
                    |> Validate.andMap street_kind.valid
                    |> Validate.andMap street.valid
                    |> Validate.andMap zip_code.valid
                    |> Validate.andMap city.valid
        , view =
            \_ street_kind street zip_code city ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Element.row [ Element.spacing 20 ]
                        [ Widgets.dropdownSelectView
                            { options = streetKindEnum.values
                            , dropdownOptions = []
                            , itemToPrompt = streetKindLabel >> Element.text
                            , itemToElement = \_ _ -> streetKindLabel >> Element.text
                            }
                            street_kind
                        , Widgets.textInput street
                        ]
                    , Element.row [ Element.spacing 20 ]
                        [ Widgets.textInput zip_code
                        , Widgets.textInput city
                        ]
                    ]
        }
        |> Form.fieldWithState "street_kind" (Field.custom streetKindEnum.toString streetKindEnum.fromString) Widgets.dropdownSelect
        |> Form.field "street" Field.text
        |> Form.field "zip_code" Field.text
        |> Form.field "city" Field.text


form =
    group
        |> Form.finalize
