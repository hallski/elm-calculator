module Op exposing (..)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type Op = None | Add | Minus | Multi | Div | Eql


executeOp : Op -> Float -> Float -> Float
executeOp op lhs rhs =
    case op of
        Add ->
            lhs + rhs

        Minus ->
            lhs - rhs

        Multi ->
            lhs * rhs

        Div ->
            lhs / rhs

        Eql ->
            lhs

        None ->
            lhs

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
