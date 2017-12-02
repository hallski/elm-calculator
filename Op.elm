module Op exposing (..)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type Op = None | Add | Minus | Multi | Div | Eql


executeOp : Op -> Result String Float -> Float -> Result String Float
executeOp op result rhs =
    Result.andThen (eo op rhs) result

eo op rhs lhs =
    case (op, rhs) of
        (Add, rhs) ->
            Ok <| lhs + rhs

        (Minus, rhs) ->
            Ok <| lhs - rhs

        (Multi, rhs) ->
            Ok <| lhs * rhs

        (Div, 0) ->
            Err "Inf"

        (Div, rhs) ->
            Ok <| lhs / rhs

        (Eql, _) ->
            Ok lhs

        (None, _) ->
            Ok lhs

toString : Op -> String
toString op =
    case op of
        Add ->
            "+"

        Minus ->
            "-"

        Multi ->
            "*"

        Div ->
            "/"

        Eql ->
            "="

        None ->
            ""

viewOpButton : (Op -> msg) -> Op -> Html msg
viewOpButton msg op =
    button [ onClick (msg op) ] [ text (toString op) ]
