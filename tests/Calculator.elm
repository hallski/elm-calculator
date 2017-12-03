module Calculator exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, float, list, string)
import Test exposing (..)

import Op

suite : Test
suite =
    describe "Operations"
        [ fuzz2 float float "add" <|
            \x y ->
                let
                    result = x + y
                in
                    Op.executeOp Op.Add (Ok x) y
                      |> Expect.equal (Ok result)

        , fuzz2 float float "subtract" <|
            \x y ->
                let
                    result = x - y
                in
                    Op.executeOp Op.Subtract (Ok x) y
                      |> Expect.equal (Ok result)
        , fuzz2 float float "multiply" <|
            \x y ->
                let
                    result = x * y
                in
                    Op.executeOp Op.Multi (Ok x) y
                      |> Expect.equal (Ok result)
        ]
