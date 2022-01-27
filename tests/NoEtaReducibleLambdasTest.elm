module NoEtaReducibleLambdasTest exposing (all)

import NoEtaReducibleLambdas exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


type alias ExpectedResult =
    { under : String, newSource : List String }


expectedErrorUnder : { message : String, details : List String } -> ExpectedResult -> Review.Test.ExpectedError
expectedErrorUnder message { under, newSource } =
    Review.Test.error
        { message = message.message
        , details = message.details
        , under = under
        }
        |> Review.Test.whenFixed (sourceCode newSource)


canReduceToJustFunctionApplicationUnder : ExpectedResult -> Review.Test.ExpectedError
canReduceToJustFunctionApplicationUnder =
    expectedErrorUnder NoEtaReducibleLambdas.canReduceToJustFunctionApplication


canReduceFunctionArgumentsUnder : ExpectedResult -> Review.Test.ExpectedError
canReduceFunctionArgumentsUnder =
    expectedErrorUnder NoEtaReducibleLambdas.canReduceFunctionArguments


expectError : Review.Test.ExpectedError -> String -> String -> Test
expectError err why s =
    test why <|
        \_ ->
            s |> Review.Test.run rule |> Review.Test.expectErrors [ err ]


expectCanReduceFunctionArguments : { description : String, source : List String } -> ExpectedResult -> Test
expectCanReduceFunctionArguments { description, source } result =
    expectError (canReduceFunctionArgumentsUnder result) description (sourceCode source)


expectCanReduceToJustFunctionApplication : { description : String, source : List String } -> ExpectedResult -> Test
expectCanReduceToJustFunctionApplication { description, source } result =
    expectError (canReduceToJustFunctionApplicationUnder result) description (sourceCode source)


expectNoErrorWhen : String -> List String -> Test
expectNoErrorWhen when s =
    test when <|
        \() ->
            sourceCode s
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors


sourceCode : List String -> String
sourceCode =
    (::) "module A exposing (..)" >> String.join "\n"


all : Test
all =
    describe "NoEtaReducibleLambdas"
        [ describe "should report an error"
            [ expectCanReduceFunctionArguments
                { description = "arguments can be removed but the lambda not entirely when a recursive call"
                , source =
                    [ "f a b ="
                    , "    (\\a b -> f a b)"
                    ]
                }
                { under = "\\a b -> f a b"
                , newSource =
                    [ "f a b ="
                    , "    (\\a -> f a)"
                    ]
                }
            , expectCanReduceFunctionArguments
                { description = "only arguments not involved in computations can be removed"
                , source =
                    [ "f a b ="
                    , "    (\\a b -> g (a 10) b)"
                    ]
                }
                { under = "\\a b -> g (a 10) b"
                , newSource =
                    [ "f a b ="
                    , "    (\\a -> g (a 10))"
                    ]
                }
            , expectCanReduceToJustFunctionApplication
                { description = "when all argugments can be removed from a non-recursive call"
                , source =
                    [ "f a b ="
                    , "    (\\a b -> getAwesomeValue a b)"
                    ]
                }
                { under = "\\a b -> getAwesomeValue a b"
                , newSource =
                    [ "f a b ="
                    , "    (getAwesomeValue)"
                    ]
                }
            , expectCanReduceToJustFunctionApplication
                { description = "when all argugments can be removed from a non-recursive call nested inside a let"
                , source =
                    [ "f x y ="
                    , "    let"
                    , "        result ="
                    , "            List.filter (\\a b -> z a b) someStuff"
                    , "    in"
                    , "    result"
                    ]
                }
                { under = "\\a b -> z a b"
                , newSource =
                    [ "f x y ="
                    , "    let"
                    , "        result ="
                    , "            List.filter (z) someStuff"
                    , "    in"
                    , "    result"
                    ]
                }
            , test
                "multiple reductions in the same source"
              <|
                \_ ->
                    sourceCode
                        [ "f x y ="
                        , "    let"
                        , "        -- can remove all "
                        , "        result ="
                        , "            List.filter (\\a b -> z a b) someStuff"
                        , "        -- can remove all "
                        , "        result2 ="
                        , "            List.filter (\\a b -> zanzibar a b) someStuff"
                        , "        -- can remove some"
                        , "        result3 ="
                        , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                        , "    in"
                        , "    result"
                        ]
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ canReduceToJustFunctionApplicationUnder
                                { newSource =
                                    [ "f x y ="
                                    , "    let"
                                    , "        -- can remove all "
                                    , "        result ="
                                    , "            List.filter (z) someStuff"
                                    , "        -- can remove all "
                                    , "        result2 ="
                                    , "            List.filter (\\a b -> zanzibar a b) someStuff"
                                    , "        -- can remove some"
                                    , "        result3 ="
                                    , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                , under = "\\a b -> z a b"
                                }
                            , canReduceToJustFunctionApplicationUnder
                                { newSource =
                                    [ "f x y ="
                                    , "    let"
                                    , "        -- can remove all "
                                    , "        result ="
                                    , "            List.filter (\\a b -> z a b) someStuff"
                                    , "        -- can remove all "
                                    , "        result2 ="
                                    , "            List.filter (zanzibar) someStuff"
                                    , "        -- can remove some"
                                    , "        result3 ="
                                    , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                , under = "\\a b -> zanzibar a b"
                                }
                            , canReduceFunctionArgumentsUnder
                                { newSource =
                                    [ "f x y ="
                                    , "    let"
                                    , "        -- can remove all "
                                    , "        result ="
                                    , "            List.filter (\\a b -> z a b) someStuff"
                                    , "        -- can remove all "
                                    , "        result2 ="
                                    , "            List.filter (\\a b -> zanzibar a b) someStuff"
                                    , "        -- can remove some"
                                    , "        result3 ="
                                    , "            List.filter (\\a -> zanzibar (a 10)) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                , under = "\\a b -> zanzibar (a 10) b"
                                }
                            ]
            ]
        , describe "should not report an error"
            [ expectNoErrorWhen "arguments are flipped"
                [ "f a b ="
                , "    (\\a b -> g b a)"
                ]
            , expectNoErrorWhen "arguments involve computations"
                [ "f a b ="
                , "    (\\a b -> g (a 10) (b 10))"
                ]
            , expectNoErrorWhen "an argument is used multiple times in applicants"
                [ "f a ="
                , "    (\\z -> g z z)"
                ]
            , expectNoErrorWhen "an argument is used multiple times in a nested expression"
                [ "f a ="
                , "    (\\z -> g (z z))"
                ]
            ]
        ]
