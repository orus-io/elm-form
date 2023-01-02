module PersonForm exposing (..)

import Element
import Form.Builder as Builder
import Form.Field as Field
import Form.Validate as Validate
import Widgets


type alias Person =
    { firstname : String
    , lastname : String
    , phoneNumbers : List String
    }


type alias FormData =
    Builder.Model String Person ()


type alias Msg =
    Builder.Msg ()


form =
    Builder.init
        { validate =
            \firstname lastname _ ->
                Validate.succeed Person
                    |> Validate.andMap firstname.valid
                    |> Validate.andMap lastname.valid
                    |> Validate.andMap (Validate.succeed [])

        --|> Validate.andMap phoneNumbers.valid
        , view =
            \firstname lastname _ ->
                Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.centerY
                    ]
                    [ Widgets.textInput firstname
                    , Widgets.textInput lastname
                    ]
        }
        |> Builder.field "firstname" (Field.text |> Field.withInitialValue .firstname)
        |> Builder.field "lastname" (Field.text |> Field.withInitialValue .lastname)
        |> Builder.finalize
