module UtilsTests exposing (..)

import Expect
import Test exposing (..)
import Utils exposing (..)


suite : Test
suite =
    describe "Utils"
        [ describe "splitOnEmptyLine"
            [ test "splits as expected" <|
                \_ ->
                    let
                        input =
                            """andrew
healy

is

great"""

                        expected =
                            [ [ "andrew", "healy" ], [ "is" ], [ "great" ] ]
                    in
                    Expect.equal (splitOnEmptyLine (String.lines input)) expected
            ]
        ]
