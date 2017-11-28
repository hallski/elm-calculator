module Calculator exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)

import Json.Decode as Json


-- Model
type Op = None | Add | Minus | Multi | Div | Eql

type alias Model =
    { currentInput : String
    , result : Float
    , currentOp : Op
    }

init : ( Model, Cmd Msg )
init =
    ( Model "" 0 None, Cmd.none )


-- Update
type Msg
    = NewInput (String)
    | NextOperator (Op)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput txt ->
            ( { model | currentInput = txt }, Cmd.none )
        NextOperator op ->
            if model.currentOp == None then
                ( { model | currentOp = op, result = parseFloat model.currentInput, currentInput = "" }, Cmd.none)
            else
                let
                    result = executeOp model.currentOp model.result (parseFloat model.currentInput)
                in
                    ({ model | result = result, currentOp = op, currentInput = "" }, Cmd.none)

parseFloat : String -> Float
parseFloat = Result.withDefault 0.0 << String.toFloat

executeOp : Op -> Float -> Float -> Float
executeOp op lhs rhs =
    case op of
        Add -> lhs + rhs
        Minus -> lhs - rhs
        Multi -> lhs * rhs
        Div -> lhs / rhs
        Eql -> lhs
        None -> lhs

-- View
viewButtonRow : Html Msg
viewButtonRow =
    div []
        [ viewOpButton (NextOperator Add) "+"
        , viewOpButton (NextOperator Minus) "-"
        , viewOpButton (NextOperator Multi) "*"
        , viewOpButton (NextOperator Div) "/"
        , viewOpButton (NextOperator Eql) "="
        ]

viewOpButton : Msg -> String -> Html Msg
viewOpButton msg name =
    button [ onClick msg ] [ text name ]

view : Model -> Html Msg
view model =
    div [ ]
        [ div [ class "result" ] [ toString model.result |> text ]
        , opToText model.currentOp
        , input
            [ onEnter (NextOperator Eql)
            , onInput NewInput
            , value model.currentInput
            , autofocus True
            ]
            []
        , viewButtonRow
       ]

opToText : Op -> Html Msg
opToText op =
    let
        opString = case op of
            Add -> "+"
            Minus -> "-"
            Multi -> "*"
            Div -> "/"
            _ -> ""
    in
        text opString

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not Enter"
    in
        on "keydown" (Json.andThen isEnter keyCode)

-- Main

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions  = \_ -> Sub.none
        }