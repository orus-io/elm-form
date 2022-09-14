module AddressForm exposing (..)

import Element
import Element.Input as Input
import Enum exposing (Enum)
import Form exposing (Form)
import Form.Builder as Builder
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type StreetKind
    = Avenue
    | Boulevard
    | Street


type alias FormData =
    Form String Address


type alias Msg =
    Form.Msg


streetKindEnum : Enum StreetKind
streetKindEnum =
    Enum.enum
        [ ( Avenue, "ave" )
        , ( Boulevard, "bld" )
        , ( Street, "str" )
        ]


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
                    |> Validate.andMap
                        (Validate.field street_kind Validate.string
                            |> Validate.andThen
                                (streetKindEnum.fromString
                                    >> Maybe.map Validate.succeed
                                    >> Maybe.withDefault (Validate.fail <| Validate.customError "")
                                )
                        )
                    |> Validate.andMap (Validate.field street Validate.string)
                    |> Validate.andMap (Validate.field zip_code Validate.string)
                    |> Validate.andMap (Validate.field city Validate.string)
        , view =
            \street_kind street zip_code city _ ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.radio []
                        { options =
                            [ Input.option Avenue <| Element.text "Avenue"
                            , Input.option Boulevard <| Element.text "Boulevard"
                            , Input.option Street <| Element.text "Street"
                            ]
                        , label = Input.labelHidden "Type de rue"
                        }
                        street_kind
                    ]
        }
        |> Builder.field "street_kind" (Field.custom streetKindEnum.toString streetKindEnum.fromString)
        |> Builder.field "street" Field.text
        |> Builder.field "zip_code" Field.text
        |> Builder.field "city" Field.text
        |> Builder.finalize
