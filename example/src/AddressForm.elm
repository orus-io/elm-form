module AddressForm exposing (..)

import Dropdown
import Element
import Element.Input as Input
import Enum exposing (Enum)
import Form exposing (Form)
import Form.Builder as Builder
import Form.Field as Field
import Form.FieldStack
import Form.Validate as Validate
import Widgets


type StreetKind
    = Avenue
    | Boulevard
    | Street


type alias FormData =
    Builder.Model String Address ( Dropdown.State StreetKind, () )


type alias Msg =
    Builder.Msg (Form.FieldStack.Msg (Widgets.DropdownMsg StreetKind) ())


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


form =
    Builder.init
        { validate =
            \street_kind street zip_code city _ ->
                Validate.succeed Address
                    |> Validate.andMap street_kind.valid
                    |> Validate.andMap street.valid
                    |> Validate.andMap zip_code.valid
                    |> Validate.andMap city.valid
        , view =
            \street_kind street zip_code city _ ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.dropdownSelectView
                        { options = streetKindEnum.values
                        , dropdownOptions = []
                        , itemToPrompt = streetKindLabel >> Element.text
                        , itemToElement = \_ _ -> streetKindLabel >> Element.text
                        }
                        street_kind
                    ]
        }
        |> Builder.fieldWithState "street_kind" (Field.custom streetKindEnum.toString streetKindEnum.fromString) Widgets.dropdownSelect
        |> Builder.field "street" Field.text
        |> Builder.field "zip_code" Field.text
        |> Builder.field "city" Field.text
        |> Builder.finalize
