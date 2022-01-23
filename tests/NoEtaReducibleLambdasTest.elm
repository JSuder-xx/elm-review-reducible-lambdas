module NoEtaReducibleLambdasTest exposing (all)

import NoEtaReducibleLambdas exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoEtaReducibleLambdas"
        [ test "should report an error when argugments can be removed from a recursive call" <|
            \() ->
                """module A exposing (..)
f a b =
    (\\a b -> f a b)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You can reduce"
                            , details = [ "x" ]
                            , under = "\\a b -> f a b"
                            }
                        ]
        , test "should report an error when argugments can be removed from a non-recursive call" <|
            \() ->
                """module A exposing (..)
f a b =
    (\\a b -> g a b)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You can reduce to just a function"
                            , details = [ "x" ]
                            , under = "\\a b -> g a b"
                            }
                        ]
        , test "should report an error when argugments can be removed from a non-recursive call nested inside a let" <|
            \() ->
                """module A exposing (..)
f x y =
    let
        result =
            List.filter (\\a b -> z a b) someStuff
    in
    result
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "You can reduce to just a function"
                            , details = [ "x" ]
                            , under = "\\a b -> z a b"
                            }
                        ]
        , test "should not report an error when arguments are flipped" <|
            \() ->
                """module A exposing (..)
f a b =
    (\\a b -> g b a)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when an argument is used multiple times in applicants" <|
            \() ->
                """module A exposing (..)
f a = 
    (\\z -> g z z)   
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when an argument is used multiple times in a nested expression" <|
            \() ->
                """module A exposing (..)
f a = 
    (\\z -> g (z z))   
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
