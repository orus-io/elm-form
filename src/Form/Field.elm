module Form.Field exposing
    ( Field, FieldValue(..), value, string, bool, group, list, custom
    , asString, asBool
    , FieldDef(..), boolean, file, text, withInitialValue
    )

{-| Read and write field values.


# Constructors

@docs Field, FieldValue, value, string, bool, group, list, custom


# Value readers

@docs asString, asBool

-}

import File exposing (File)
import Form.Tree as Tree exposing (Tree)


type FieldDef data a
    = FieldDef (Maybe (data -> a)) (a -> FieldValue) (Field -> Maybe a)


text : FieldDef data String
text =
    FieldDef Nothing String asString


boolean : FieldDef data Bool
boolean =
    FieldDef Nothing Bool asBool


file : FieldDef data File
file =
    FieldDef Nothing File asFile


custom : (option -> String) -> (String -> Maybe option) -> FieldDef data option
custom toString fromString =
    FieldDef Nothing (toString >> String) (asString >> Maybe.andThen fromString)


withInitialValue : (data -> a) -> FieldDef data a -> FieldDef data a
withInitialValue load (FieldDef _ toField toFieldValue) =
    FieldDef (Just load) toField toFieldValue


{-| A field is a tree node.
-}
type alias Field =
    Tree FieldValue


{-| Form field. Can either be a group of named fields, or a final field.
-}
type FieldValue
    = String String
    | Bool Bool
    | File File
    | EmptyField


{-| Build a field from its value.
-}
value : FieldValue -> Field
value =
    Tree.Value


{-| Build a string field, for text inputs, selects, etc.
-}
string : String -> Field
string =
    String >> Tree.Value


{-| Build a boolean field, for checkboxes.
-}
bool : Bool -> Field
bool =
    Bool >> Tree.Value


{-| Gather named fields as a group field.
-}
group : List ( String, Field ) -> Field
group =
    Tree.group


{-| Gather fields as a list field.
-}
list : List Field -> Field
list =
    Tree.List


{-| Get field value as boolean.
-}
asBool : Field -> Maybe Bool
asBool field =
    case field of
        Tree.Value (Bool b) ->
            Just b

        _ ->
            Nothing


{-| Get field value as file.
-}
asFile : Field -> Maybe File
asFile field =
    case field of
        Tree.Value (File b) ->
            Just b

        _ ->
            Nothing


{-| Get field value as string.
-}
asString : Field -> Maybe String
asString field =
    case field of
        Tree.Value (String s) ->
            Just s

        _ ->
            Nothing
