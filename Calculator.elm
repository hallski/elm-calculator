module Calculator exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Op
import Events


-- Model

type alias Model =
    { currentInput : String
    , result : Float
    , currentOp : Op.Op
    }

init : ( Model, Cmd Msg )
init =
    ( Model "" 0 Op.None, Cmd.none )


-- Update
type Msg
    = NewInput (String)
    | NextOperator (Op.Op)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput txt ->
            ( { model | currentInput = txt }, Cmd.none )
        NextOperator op ->
            handleNextOp model op

parseFloat : String -> Float
parseFloat = Result.withDefault 0.0 << String.toFloat

handleNextOp : Model -> Op.Op -> ( Model, Cmd Msg )
handleNextOp model op =
    if model.currentOp == Op.None then
        ( { model | currentOp = op, result = parseFloat model.currentInput, currentInput = "" }, Cmd.none )
    else
        let
            result = parseFloat model.currentInput
                        |> Op.executeOp model.currentOp model.result
        in
            ({ model | result = result, currentOp = op, currentInput = "" }, Cmd.none)


-- View
viewButtonRow : Html Msg
viewButtonRow =
    div []
        [ Op.viewOpButton NextOperator Op.Add
        , Op.viewOpButton NextOperator Op.Minus
        , Op.viewOpButton NextOperator Op.Multi
        , Op.viewOpButton NextOperator Op.Div
        , Op.viewOpButton NextOperator Op.Eql
        ]

view : Model -> Html Msg
view model =
    div [ ]
        [ div [ class "result" ] [ toString model.result |> text ]
        , Op.toString model.currentOp |> text
        , input
            [ Events.onEnter (NextOperator Op.Eql)
            , onInput NewInput
            , value model.currentInput
            , autofocus True
            ]
            []
        , viewButtonRow
       ]


-- Main

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions  = \_ -> Sub.none
        }
