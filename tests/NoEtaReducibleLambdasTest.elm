module NoEtaReducibleLambdasTest exposing (all)

import NoEtaReducibleLambdas exposing (Config, LambdaReduceStrategy(..), rule)
import Ra
import Review.Test
import Test exposing (Test, describe, test)


type alias ExpectedResult =
    { under : String, newSource : List String }


unrestrictedArgument : LambdaReduceStrategy -> Config
unrestrictedArgument strat =
    { lambdaReduceStrategy = strat
    , argumentNamePredicate = always True
    }


singleLetterArgument : LambdaReduceStrategy -> Config
singleLetterArgument strat =
    { lambdaReduceStrategy = strat
    , argumentNamePredicate = String.length >> Ra.lessThanEqualTo 1
    }


expectedErrorUnder : { message : String, details : List String } -> ExpectedResult -> Review.Test.ExpectedError
expectedErrorUnder message { under, newSource } =
    Review.Test.error
        { message = message.message
        , details = message.details
        , under = under
        }
        |> Review.Test.whenFixed (sourceCode newSource)


canRemoveSomeArguments : ExpectedResult -> Review.Test.ExpectedError
canRemoveSomeArguments =
    expectedErrorUnder NoEtaReducibleLambdas.canRemoveSomeArguments


canRemoveLambda : ExpectedResult -> Review.Test.ExpectedError
canRemoveLambda =
    expectedErrorUnder NoEtaReducibleLambdas.canRemoveLambda


canReduceToIdentity : ExpectedResult -> Review.Test.ExpectedError
canReduceToIdentity =
    expectedErrorUnder NoEtaReducibleLambdas.reducesToIdentity


type alias TestConfig =
    { description : String, source : List String }


expectError : TestConfig -> Review.Test.ExpectedError -> Config -> Test
expectError { description, source } err config =
    test ("WHEN " ++ Debug.toString config ++ " then " ++ description) <|
        \_ ->
            source |> sourceCode |> Review.Test.run (rule config) |> Review.Test.expectErrors [ err ]


expectCanRemoveSomeArguments : TestConfig -> ExpectedResult -> Config -> Test
expectCanRemoveSomeArguments config result =
    expectError config (canRemoveSomeArguments result)


expectCanRemoveLambda : TestConfig -> ExpectedResult -> Config -> Test
expectCanRemoveLambda config result =
    expectError config (canRemoveLambda result)


expectCanReduceToIdentity : TestConfig -> ExpectedResult -> Config -> Test
expectCanReduceToIdentity config result =
    expectError config (canReduceToIdentity result)


expectNoErrorWhen : String -> List String -> Config -> Test
expectNoErrorWhen when s strat =
    test ("WHEN " ++ Debug.toString strat ++ " then " ++ when) <|
        \() ->
            sourceCode s
                |> Review.Test.run (rule strat)
                |> Review.Test.expectNoErrors


sourceCode : List String -> String
sourceCode =
    (::) "module A exposing (..)" >> String.join "\n"


allConfigs : List LambdaReduceStrategy
allConfigs =
    [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication, OnlyWhenSingleArgument ]


all : Test
all =
    describe "NoEtaReducibleLambdas"
        [ describe "should report an error" <|
            List.concat
                [ [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map unrestrictedArgument
                    |> List.map
                        (expectCanRemoveSomeArguments
                            { description = "arguments can be removed but the lambda cannot because of value recursion"
                            , source =
                                [ "f ="
                                , "    (\\a b -> f a b)"
                                ]
                            }
                            { under = "\\a b -> f a b"
                            , newSource =
                                [ "f ="
                                , "    (\\a -> f a)"
                                ]
                            }
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "can reduce to function outright"
                                , source =
                                    [ "f x y ="
                                    , "    let"
                                    , "        g ="
                                    , "            \\apples bananas -> f apples bananas"
                                    , "    in"
                                    , "    g x y"
                                    ]
                                }
                                { under = "\\apples bananas -> f apples bananas"
                                , newSource =
                                    [ "f x y ="
                                    , "    let"
                                    , "        g ="
                                    , "            f"
                                    , "    in"
                                    , "    g x y"
                                    ]
                                }
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "can reduce to function outright even when the lambda is on two lines"
                                , source =
                                    [ "f x y ="
                                    , "    let"
                                    , "        g ="
                                    , "            \\a b ->"
                                    , "                 f a b"
                                    , "    in"
                                    , "    g x y"
                                    ]
                                }
                                { under = "\\a b ->\n                 f a b"
                                , newSource =
                                    [ "f x y ="
                                    , "    let"
                                    , "        g ="
                                    , "            f"
                                    , "    in"
                                    , "    g x y"
                                    ]
                                }
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "can reduce to function outright even when the lambda is on multiple lines"
                                , source =
                                    [ "f x y ="
                                    , "    let"
                                    , "        g ="
                                    , "            \\a b ->"
                                    , "                 f"
                                    , "                     a"
                                    , "                     b"
                                    , "    in"
                                    , "    g x y"
                                    ]
                                }
                                { under = "\\a b ->\n                 f\n                     a\n                     b"
                                , newSource =
                                    [ "f x y ="
                                    , "    let"
                                    , "        g ="
                                    , "            f"
                                    , "                    "
                                    , "                    "
                                    , "    in"
                                    , "    g x y"
                                    ]
                                }
                        )
                , [ OnlyWhenSingleArgument ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "can reduce a single argument"
                                , source =
                                    [ "f x ="
                                    , "    (\\a -> g a)"
                                    ]
                                }
                                { under = "\\a -> g a"
                                , newSource =
                                    [ "f x ="
                                    , "    (g)"
                                    ]
                                }
                        )
                , [ RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveSomeArguments
                                { description = "will not reduce to function when there are applications inside the lambda"
                                , source =
                                    [ "f x ="
                                    , "    (\\a b -> f (g 10) a b)"
                                    ]
                                }
                                { under = "\\a b -> f (g 10) a b"
                                , newSource =
                                    [ "f x ="
                                    , "    (\\a -> f (g 10) a)"
                                    ]
                                }
                        )
                , [ AlwaysRemoveLambdaWhenPossible ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "will reduce to function even when there are applications inside the lambda"
                                , source =
                                    [ "f x ="
                                    , "    (\\a b -> f (g 10) a b)"
                                    ]
                                }
                                { under = "\\a b -> f (g 10) a b"
                                , newSource =
                                    [ "f x ="
                                    , "    (f (g 10))"
                                    ]
                                }
                        )
                , [ RemoveLambdaWhenNoCallsInApplication, AlwaysRemoveLambdaWhenPossible ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "can reduce to function when function takes literal values"
                                , source =
                                    [ "f x ="
                                    , "    (\\a b -> f [1, 2] a b)"
                                    ]
                                }
                                { under = "\\a b -> f [1, 2] a b"
                                , newSource =
                                    [ "f x ="
                                    , "    (f [1, 2])"
                                    ]
                                }
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveSomeArguments
                                { description = "only arguments not involved in computations can be removed"
                                , source =
                                    [ "f ="
                                    , "    (\\a b c -> g (a 10) (b 20) c)"
                                    ]
                                }
                                { under = "\\a b c -> g (a 10) (b 20) c"
                                , newSource =
                                    [ "f ="
                                    , "    (\\a b -> g (a 10) (b 20))"
                                    ]
                                }
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanReduceToIdentity
                                { description = "reduce to identity"
                                , source =
                                    [ "f ="
                                    , "    (\\a b -> a b)"
                                    ]
                                }
                                { under = "\\a b -> a b"
                                , newSource =
                                    [ "f ="
                                    , "    (identity)"
                                    ]
                                }
                        )
                , [ RemoveLambdaWhenNoCallsInApplication, AlwaysRemoveLambdaWhenPossible ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveLambda
                                { description = "can reduce to function inside a let"
                                , source =
                                    [ "f someStuff ="
                                    , "    let"
                                    , "        result ="
                                    , "            List.filter (\\a b -> z a b) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                }
                                { under = "\\a b -> z a b"
                                , newSource =
                                    [ "f someStuff ="
                                    , "    let"
                                    , "        result ="
                                    , "            List.filter (z) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                }
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (unrestrictedArgument
                            >> expectCanRemoveSomeArguments
                                { description = "can reduce arguments inside a let"
                                , source =
                                    [ "f ="
                                    , "    let"
                                    , "        result ="
                                    , "            List.filter (\\a b -> f a b) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                }
                                { under = "\\a b -> f a b"
                                , newSource =
                                    [ "f ="
                                    , "    let"
                                    , "        result ="
                                    , "            List.filter (\\a -> f a) someStuff"
                                    , "    in"
                                    , "    result"
                                    ]
                                }
                        )
                , [ test
                        "multiple reductions in the same source with an outer value"
                    <|
                        \_ ->
                            sourceCode
                                [ "f ="
                                , "    let"
                                , "        result1 ="
                                , "            List.filter (\\a b -> f a b) someStuff"
                                , "        result2 ="
                                , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                                , "        result3 ="
                                , "            List.filter (\\a b -> zanzibar a b) someStuff"
                                , "        result4 ="
                                , "            List.filter (\\a b -> result4 a b) someStuff"
                                , "    in"
                                , "    result"
                                ]
                                |> Review.Test.run (rule (unrestrictedArgument RemoveLambdaWhenNoCallsInApplication))
                                |> Review.Test.expectErrors
                                    [ canRemoveSomeArguments
                                        { newSource =
                                            [ "f ="
                                            , "    let"
                                            , "        result1 ="
                                            , "            List.filter (\\a -> f a) someStuff"
                                            , "        result2 ="
                                            , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                                            , "        result3 ="
                                            , "            List.filter (\\a b -> zanzibar a b) someStuff"
                                            , "        result4 ="
                                            , "            List.filter (\\a b -> result4 a b) someStuff"
                                            , "    in"
                                            , "    result"
                                            ]
                                        , under = "\\a b -> f a b"
                                        }
                                    , canRemoveSomeArguments
                                        { newSource =
                                            [ "f ="
                                            , "    let"
                                            , "        result1 ="
                                            , "            List.filter (\\a b -> f a b) someStuff"
                                            , "        result2 ="
                                            , "            List.filter (\\a -> zanzibar (a 10)) someStuff"
                                            , "        result3 ="
                                            , "            List.filter (\\a b -> zanzibar a b) someStuff"
                                            , "        result4 ="
                                            , "            List.filter (\\a b -> result4 a b) someStuff"
                                            , "    in"
                                            , "    result"
                                            ]
                                        , under = "\\a b -> zanzibar (a 10) b"
                                        }
                                    , canRemoveLambda
                                        { newSource =
                                            [ "f ="
                                            , "    let"
                                            , "        result1 ="
                                            , "            List.filter (\\a b -> f a b) someStuff"
                                            , "        result2 ="
                                            , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                                            , "        result3 ="
                                            , "            List.filter (zanzibar) someStuff"
                                            , "        result4 ="
                                            , "            List.filter (\\a b -> result4 a b) someStuff"
                                            , "    in"
                                            , "    result"
                                            ]
                                        , under = "\\a b -> zanzibar a b"
                                        }
                                    , canRemoveSomeArguments
                                        { newSource =
                                            [ "f ="
                                            , "    let"
                                            , "        result1 ="
                                            , "            List.filter (\\a b -> f a b) someStuff"
                                            , "        result2 ="
                                            , "            List.filter (\\a b -> zanzibar (a 10) b) someStuff"
                                            , "        result3 ="
                                            , "            List.filter (\\a b -> zanzibar a b) someStuff"
                                            , "        result4 ="
                                            , "            List.filter (\\a -> result4 a) someStuff"
                                            , "    in"
                                            , "    result"
                                            ]
                                        , under = "\\a b -> result4 a b"
                                        }
                                    ]
                  ]
                ]
        , describe "should not report an error" <|
            List.concat
                [ allConfigs
                    |> List.map
                        (unrestrictedArgument
                            >> expectNoErrorWhen "arguments are flipped"
                                [ "f a b ="
                                , "    (\\a b -> g b a)"
                                ]
                        )
                , allConfigs
                    |> List.map
                        (unrestrictedArgument
                            >> expectNoErrorWhen "arguments involve computations"
                                [ "f a b ="
                                , "    (\\a b -> g (a 10) (b 10))"
                                ]
                        )
                , allConfigs
                    |> List.map
                        (unrestrictedArgument
                            >> expectNoErrorWhen "an argument is used multiple times in applicants"
                                [ "f a ="
                                , "    (\\z -> g z z)"
                                ]
                        )
                , allConfigs
                    |> List.map
                        (unrestrictedArgument
                            >> expectNoErrorWhen "an argument is used multiple times in a nested expression"
                                [ "f a ="
                                , "    (\\z -> g (z z))"
                                ]
                        )
                , [ AlwaysRemoveLambdaWhenPossible, RemoveLambdaWhenNoCallsInApplication ]
                    |> List.map
                        (singleLetterArgument
                            >> expectNoErrorWhen "an argument fails the predicate"
                                [ "f x y ="
                                , "    let"
                                , "        g ="
                                , "            \\apples bananas -> f apples bananas"
                                , "    in"
                                , "    g x y"
                                ]
                        )
                , [ expectNoErrorWhen "Multiple arguments and configured to only work with single arguments"
                        [ "f a ="
                        , "  (\\x y -> g x y)"
                        ]
                        (unrestrictedArgument OnlyWhenSingleArgument)
                  , expectNoErrorWhen "Multiple arguments (with one application) and configured to only work with single arguments"
                        [ "f a ="
                        , "  (\\x y -> g (x 10) y)"
                        ]
                        (unrestrictedArgument OnlyWhenSingleArgument)
                  ]
                ]
        ]
