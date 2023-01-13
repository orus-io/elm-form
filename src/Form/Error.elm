module Form.Error exposing (Error, value)

{-| Validation errors.

@docs Error, ErrorValue, value

-}

import Form.Tree as Tree exposing (Tree)


{-| Tree of errors.
-}
type alias Error e =
    Tree (List e)


{-| Build a tree node (a leaf) for this error
-}
value : a -> Error a
value =
    List.singleton
        >> Tree.Value
