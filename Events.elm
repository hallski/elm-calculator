module Events exposing (..)

import Html.Events exposing (on, keyCode)
import Html exposing (Attribute)
import Json.Decode as Json


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not Enter"
    in
        on "keydown" (Json.andThen isEnter keyCode)
