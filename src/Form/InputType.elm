module Form.InputType exposing (InputType(..))

{-|

@docs InputType

-}


{-| Input types to determine live validation behaviour.
-}
type InputType
    = Text
    | Textarea
    | Select
    | Radio
    | Checkbox
