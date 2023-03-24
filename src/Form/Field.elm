module Form.Field exposing
    ( FieldDef(..), text, boolean, file, custom, withInitialValue
    , Field, FieldValue(..), value, string, bool, group, list
    , asString, asBool
    )

{-| Read and write field values.


# Field definition

@docs FieldDef, text, boolean, file, custom, withInitialValue


# Constructors

@docs Field, FieldValue, value, string, bool, group, list


# Value readers

@docs asString, asBool

-}

import File exposing (File)
import Form.Tree as Tree exposing (Tree)


{-| A field definition, can be added to a form builder
-}
type FieldDef data a
    = FieldDef (Maybe (data -> a)) (a -> FieldValue) (Field -> Maybe a)


{-| returns a field definition that store a String value
-}
text : FieldDef data String
text =
    FieldDef Nothing String asString


{-| returns a field definition that store a Bool value
-}
boolean : FieldDef data Bool
boolean =
    FieldDef Nothing Bool asBool


{-| returns a field definition that store a File value
-}
file : FieldDef data File
file =
    FieldDef Nothing File asFile


{-| returns a field definition that store a custom type value
-}
custom : (option -> String) -> (String -> Maybe option) -> FieldDef data option
custom toString fromString =
    FieldDef Nothing (toString >> String) (asString >> Maybe.andThen fromString)


{-| add a initial value loader to a field definition
-}
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
