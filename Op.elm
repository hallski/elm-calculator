module Op exposing (..)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type Op
    = None
    | Add
    | Minus
    | Multi
    | Div
    | Eql


divide : Float -> Float -> Result String Float
divide lhs rhs =
    case rhs of
        0 ->
            Err "Inf"

        rhs ->
            Ok <| lhs / rhs


ok : (Float -> Float -> Float) -> Float -> Float -> Result String Float
ok op lhs rhs =
    Ok <| op lhs rhs


okLhs : Float -> Float -> Result String Float
okLhs v _ =
    Ok v


getOp : Op -> (String, Float -> Float -> Result String Float)
getOp op =
    case op of
        Add ->
            ("+", ok (+))

        Minus ->
            ("-", ok (-))

        Multi ->
            ("*", ok (*))

        Div ->
            ("/", divide)

        Eql ->
            ("=", okLhs)

        None ->
            ("", okLhs)


toString : Op -> String
toString =
    Tuple.first << getOp


toFunction : Op -> (Float -> Float -> Result String Float)
toFunction =
    Tuple.second << getOp


executeOp : Op -> Result String Float -> Float -> Result String Float
executeOp op result rhs =
    let
        f = rhs |> flip (toFunction op)
    in
        Result.andThen f result


viewOpButton : (Op -> msg) -> Op -> Html msg
viewOpButton msg op =
    button [ onClick (msg op) ] [ text (toString op) ]
