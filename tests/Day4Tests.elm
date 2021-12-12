module Day4Tests exposing (..)

import Array
import Day4 exposing (parseInput)
import Expect
import Test exposing (..)


testInput : String
testInput =
    """7,4,9

22 13 17
 8  2 23
21  9 14

 3 15  0
 9 18 13
19  8  7"""


suite : Test
suite =
    describe "Day4"
        [ describe "parseInput"
            [ test "deals with test input" <|
                \_ ->
                    let
                        expected =
                            { numbers = [ 7, 4, 9 ]
                            , cards =
                                [ { squares =
                                        Array.fromList
                                            [ { value = 22
                                              , checked = False
                                              }
                                            , { value = 13
                                              , checked = False
                                              }
                                            , { value = 17
                                              , checked = False
                                              }
                                            , { value = 8
                                              , checked = False
                                              }
                                            , { value = 2
                                              , checked = False
                                              }
                                            , { value = 23
                                              , checked = False
                                              }
                                            , { value = 21
                                              , checked = False
                                              }
                                            , { value = 9
                                              , checked = False
                                              }
                                            , { value = 14
                                              , checked = False
                                              }
                                            ]
                                  , width = 3
                                  , height = 3
                                  }
                                , { squares =
                                        Array.fromList
                                            [ { value = 3
                                              , checked = False
                                              }
                                            , { value = 15
                                              , checked = False
                                              }
                                            , { value = 0
                                              , checked = False
                                              }
                                            , { value = 9
                                              , checked = False
                                              }
                                            , { value = 18
                                              , checked = False
                                              }
                                            , { value = 13
                                              , checked = False
                                              }
                                            , { value = 19
                                              , checked = False
                                              }
                                            , { value = 8
                                              , checked = False
                                              }
                                            , { value = 7
                                              , checked = False
                                              }
                                            ]
                                  , width = 3
                                  , height = 3
                                  }
                                ]
                            }
                    in
                    Expect.equal (parseInput testInput) expected
            ]
        ]
