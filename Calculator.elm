module Calculator exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Html.Events exposing (onClick, onInput)


-- Model
type Op = None | Add | Minus | Multi | Div

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
    | PlusClick | MinusClick
    | MultClick | DivClick
    | EqlClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput txt ->
            ( { model | currentInput = txt }, Cmd.none )
        PlusClick ->
            ( { model | result = model.result + parseFloat model.currentInput
                      , currentInput = ""}, Cmd.none )
        MinusClick ->
            ( { model | result = model.result - parseFloat model.currentInput
                      , currentInput = "" }, Cmd.none )
        MultClick ->
            ( { model | result = model.result * parseFloat model.currentInput
                      , currentInput = "" }, Cmd.none )
        DivClick ->
            ( { model | result = model.result / parseFloat model.currentInput
                      , currentInput = "" }, Cmd.none )
        EqlClick -> (model, Cmd.none)

parseFloat : String -> Float
parseFloat = Result.withDefault 0.0 << String.toFloat


-- View
viewButtonRow : Html Msg
viewButtonRow =
    div []
        [ viewOpButton PlusClick "+"
        , viewOpButton MinusClick "-"
        , viewOpButton MultClick "*"
        , viewOpButton DivClick "/"
        , viewOpButton EqlClick "="
        ]

viewOpButton : Msg -> String -> Html Msg
viewOpButton msg name =
    button [ onClick msg ] [ text name ]

view : Model -> Html Msg
view model =
    div [ ]
        [ div [ class "result" ] [ toString model.result |> text ]
        , input
            [ onInput NewInput
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