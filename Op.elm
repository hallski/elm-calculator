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


ok : (Float -> Float -> Float) -> (Float -> Float -> Result String Float)
ok op =
    \lhs rhs ->
        op lhs rhs |> Ok


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


executeOp : Op -> Result String Float -> Float -> Result String Float
executeOp op result rhs =
    let
        f = (flip <| Tuple.second <| getOp op) <| rhs
    in
        Result.andThen f result


toString : Op -> String
toString =
    Tuple.first << getOp


viewOpButton : (Op -> msg) -> Op -> Html msg
viewOpButton msg op =
    button [ onClick (msg op) ] [ text (toString op) ]
