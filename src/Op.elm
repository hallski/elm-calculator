module Op exposing (..)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type Op
    = None
    | Add
    | Subtract
    | Multi
    | Div
    | Eql


divide : Float -> Float -> Result String Float
divide divident divisor =
    case divisor of
        0 ->
            Err "Inf"

        _ ->
            Ok <| divident / divisor


ok : (Float -> Float -> Float) -> Float -> Float -> Result String Float
ok op x y =
    Ok <| op x y


okFirst : Float -> Float -> Result String Float
okFirst v _ =
    Ok v


getOp : Op -> (String, Float -> Float -> Result String Float)
getOp op =
    case op of
        Add ->
            ("+", ok (+))

        Subtract ->
            ("-", ok (-))

        Multi ->
            ("*", ok (*))

        Div ->
            ("/", divide)

        Eql ->
            ("=", okFirst)

        None ->
            ("", okFirst)


toString : Op -> String
toString =
    Tuple.first << getOp


toFunction : Op -> (Float -> Float -> Result String Float)
toFunction =
    Tuple.second << getOp


executeOp : Op -> Result String Float -> Float -> Result String Float
executeOp op acc value =
    let
        f = flip (toFunction op) <| value
    in
        Result.andThen f acc


viewOpButton : (Op -> msg) -> Op -> Html msg
viewOpButton msg op =
    button [ onClick <| msg op ] [ text <| toString op ]
