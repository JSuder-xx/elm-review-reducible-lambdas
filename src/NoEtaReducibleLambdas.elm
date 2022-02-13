module NoEtaReducibleLambdas exposing
    ( rule, LambdaReduceStrategy(..)
    , canRemoveLambda, canRemoveSomeArguments, reducesToIdentity
    )

{-| Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect reducible lambda expressions using different techniques.

@docs rule, LambdaReduceStrategy
@docs canRemoveLambda, canRemoveSomeArguments, reducesToIdentity

-}

import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import List.Extra
import Ra
import Range.Extra
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import VariableName


{-| Use these to control how aggressively lambda expressions are reduced.

  - OnlyWhenSingleArgument has the least impact on performance and will reduce the least.
      - This will reduce `\a -> f a` to `f`.
      - It will not reduce `\a b -> f a b` because there are two arguments.
      - It will not reduce `\a -> (getFunction 10) a` because there is a function application to get the function.

  - RemoveLambdaWhenNoCallsInApplication should have minimal impact on performance and reduces most of the functions you would want to reduce.
      - This will reduce `\a -> f a` to `f`.
      - This will reduce `\a b -> f a b` to `f`.
      - This will reduce `\a b -> f [1, 2, 3] a b` to `f [1, 2, 3]`
      - This will reduce `\a b -> f (g 10) a b` to `\a -> f (g 10) a` but it will not remove the lambda outright because of the `(g 10)` call.
      - This will reduce `\a b c -> a b c` to `identity`.
      - This will **not** reduce `\a -> f (g 10) a` due to the `(g 10)` call.

  - AlwaysRemoveLambdaWhenPossible is the most aggressive.
      - This will reduce `\a -> f a` to `f`.
      - This will reduce `\a b -> f a b` to `f`.
      - This will reduce `\a b -> f [1, 2, 3] a b` to `f [1, 2, 3]`
      - This will reduce `\a b -> f (g 10) a b` to `f (g 10)`.
      - This will reduce `\a b c -> a b c` to `identity`.
      - This will reduce `\a -> (getFunction 10) a` to `(getFunction 10)`.

See the module documentation.

-}
type LambdaReduceStrategy
    = RemoveLambdaWhenNoCallsInApplication
    | OnlyWhenSingleArgument
    | AlwaysRemoveLambdaWhenPossible


{-| This rule detect reducible lambda expressions using different techniques.


## Fail

    example1 =
        List.map (\a -> f a)

    example2 =
        List.map (\a -> f (g 10) a)


## Success

    example1 =
        List.map f

    example2 =
        List.map (f (g 10))


## When (not) to enable this rule

This rule can change the performance characteristics of your program which is why LambdaReduceStrategy offers different levels of reduction.

  - Reducing `\\a b -> f a b` to `f`
      - If `f` does work on first argument
      - Then this modification will cause that work to be done immediately on first application
      - Whereas behind the lambda it would wait until the second argument.

  - Reducing `\\a b -> f g a b` to `f g`
      - Causes an immediate application but no computation on arguments.
      - If `f` does work on first argument
      - Then this will cause that work to be done **immediately** at this location
      - And if `f` does work on second argument
      - Then this reduction will cause this work to be done on application of second argument.
      - Whereas behind this lambda the application of `g` and `a` to `f` would wait until `b` had been applied.

  - Reducing `\\a b -> f (g 10) a b` to `f (g 10)`
      - Causes `g 10` to evaluate immediately which could be costly
      - In addition to every other change indicated in the prior reduction example.


## Configuration

    module ReviewConfig exposing (config)

    import NoEtaReducibleLambdas
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoEtaReducibleLambdas.rule NoEtaReducibleLambdas.AlwaysRemoveLambdaWhenPossible
        ]


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jsuder-xx/elm-review-eta-reduction/example --rules NoEtaReducibleLambdas
```

-}
rule : LambdaReduceStrategy -> Rule
rule strategy =
    Rule.newModuleRuleSchema "NoEtaReducibleLambdas" ()
        |> Rule.withDeclarationEnterVisitor (\decl context -> ( errorsInDeclaration strategy decl, context ))
        |> Rule.fromModuleRuleSchema


type alias Errors =
    List (Rule.Error {})


functionApplication : Expression -> Maybe (List (Node Expression))
functionApplication expr =
    case expr of
        Application a ->
            Just a

        _ ->
            Nothing


nodeValues : List (Node a) -> List a
nodeValues =
    List.map Node.value


varPattern : Node Pattern -> Maybe (Node String)
varPattern (Node range p) =
    case p of
        VarPattern v ->
            Just (Node range v)

        _ ->
            Nothing


type alias ErrorMessage =
    { message : String, details : List String }


{-| Error message when arguments can be removed (for unit testing only).
-}
canRemoveSomeArguments : ErrorMessage
canRemoveSomeArguments =
    { message = "Arguments can be removed from this lambda."
    , details = [ "Arguments can be removed through the process of eta reduction." ]
    }


{-| Error message when the lambda can be removed (for unit testing only).
-}
canRemoveLambda : ErrorMessage
canRemoveLambda =
    { message = "Lambda can be removed"
    , details =
        [ "The meaning of the code remains the same even when the lambda is removed."
        , "Performance may be altered depending on the implementation of the function. In cases where the lambda is invoked many times this is likely to be a performance increase. In cases where the lambda is often never invoked (ex. mapping over a Maybe) this could decrease performance."
        ]
    }


{-| Error message when the lambda reduces to identity (for unit testing only).
-}
reducesToIdentity : ErrorMessage
reducesToIdentity =
    { message = "This lambda is actually reducible to just the identity function."
    , details = [ "After eta reduction this function finally reduced down to something of the form `\\a -> a` and that is the `identity` function." ]
    }


removeWithLeadingSpace : Range -> Fix.Fix
removeWithLeadingSpace =
    Range.Extra.expandToLeft 1 >> Fix.removeRange


removeParameterAndExpressionFixes : ( Node a, Node b ) -> List Fix.Fix
removeParameterAndExpressionFixes ( Node patternRange _, Node expressionRange _ ) =
    [ removeWithLeadingSpace patternRange
    , removeWithLeadingSpace expressionRange
    ]


removeArrow : { lastArgument : Node a, functionExpression : Node b } -> Fix.Fix
removeArrow { lastArgument, functionExpression } =
    let
        (Node lastArgumentRange _) =
            lastArgument

        (Node functionExpressionRange _) =
            functionExpression
    in
    if lastArgumentRange.end.row == functionExpressionRange.start.row then
        Fix.removeRange (Range.Extra.toTheRight { delta = 0, count = 4 } lastArgumentRange)

    else
        Fix.removeRange
            { start = { row = lastArgumentRange.end.row, column = lastArgumentRange.end.column }
            , end = { row = functionExpressionRange.start.row, column = functionExpressionRange.start.column }
            }


errorsInApplicationOfLambda : LambdaReduceStrategy -> Set String -> Range -> Errors -> List (Node Pattern) -> List (Node Expression) -> List (Rule.Error {})
errorsInApplicationOfLambda lambdaReduceStrategy valueNameSet range existingErrors arguments expressions =
    case expressions of
        [] ->
            []

        functionExpression :: _ ->
            let
                variableCounts =
                    VariableName.countInExpressions (nodeValues expressions) Dict.empty

                countOf name =
                    Dict.get name variableCounts
                        |> Maybe.withDefault 0

                ( removable, retainable ) =
                    List.map2
                        Tuple.pair
                        (List.reverse arguments)
                        (List.reverse expressions)
                        |> Ra.partitionWhile
                            (\( argumentPattern, expression ) ->
                                Maybe.map2
                                    (\argumentName variableName -> Node.value argumentName == Node.value variableName && countOf (Node.value variableName) <= 1)
                                    (varPattern argumentPattern)
                                    (VariableName.fromExpression expression)
                                    |> Maybe.withDefault False
                            )

                anyReferenceToOuterDeclaredValue _ =
                    valueNameSet
                        |> Set.toList
                        |> List.any (countOf >> Ra.greaterThan 0)

                anyExpressionIncludesApplication _ =
                    expressions |> List.any expessionIncludesApplicationNotBehindLambda

                mustKeepLambda =
                    case lambdaReduceStrategy of
                        AlwaysRemoveLambdaWhenPossible ->
                            anyReferenceToOuterDeclaredValue ()

                        RemoveLambdaWhenNoCallsInApplication ->
                            anyReferenceToOuterDeclaredValue () || anyExpressionIncludesApplication ()

                        OnlyWhenSingleArgument ->
                            anyReferenceToOuterDeclaredValue () || anyExpressionIncludesApplication ()

                ( canBeRemoved, mustBeRetained ) =
                    if mustKeepLambda && List.isEmpty retainable then
                        case List.reverse removable of
                            [] ->
                                ( removable, retainable )

                            last :: rest ->
                                ( List.reverse rest, [ last ] )

                    else
                        ( removable, retainable )

                tooManyArguments =
                    case lambdaReduceStrategy of
                        AlwaysRemoveLambdaWhenPossible ->
                            False

                        RemoveLambdaWhenNoCallsInApplication ->
                            False

                        OnlyWhenSingleArgument ->
                            List.length arguments > 1
            in
            if List.isEmpty canBeRemoved || tooManyArguments then
                existingErrors

            else if List.isEmpty mustBeRetained then
                if List.length arguments == List.length expressions then
                    case ( List.head arguments, expressions |> List.reverse |> List.head ) of
                        ( Just (Node first _), Just (Node last _) ) ->
                            let
                                newRange =
                                    { start = { row = first.start.row, column = first.start.column - 1 }
                                    , end = last.end
                                    }
                            in
                            Rule.errorWithFix
                                reducesToIdentity
                                newRange
                                [ Fix.replaceRangeBy newRange "identity" ]
                                :: existingErrors

                        _ ->
                            existingErrors

                else
                    case List.Extra.last arguments of
                        Just lastArgument ->
                            Rule.errorWithFix
                                canRemoveLambda
                                range
                                (removeArrow { lastArgument = lastArgument, functionExpression = functionExpression }
                                    :: List.concatMap
                                        removeParameterAndExpressionFixes
                                        canBeRemoved
                                )
                                :: existingErrors

                        _ ->
                            existingErrors

            else
                Rule.errorWithFix
                    canRemoveSomeArguments
                    range
                    (List.concatMap
                        removeParameterAndExpressionFixes
                        canBeRemoved
                    )
                    :: existingErrors


expessionIncludesApplicationNotBehindLambda : Node Expression -> Bool
expessionIncludesApplicationNotBehindLambda expr =
    let
        letDeclaration : Node LetDeclaration -> Bool
        letDeclaration decl =
            case Node.value decl of
                LetFunction { declaration } ->
                    let
                        { arguments, expression } =
                            Node.value declaration
                    in
                    if List.isEmpty arguments then
                        -- if there are no arguments then we have to see if this expression is going to call something
                        expessionIncludesApplicationNotBehindLambda expression

                    else
                        -- if there are arguments then the expression is behind a lambda
                        False

                LetDestructuring _ e ->
                    expessionIncludesApplicationNotBehindLambda e

        expressionsRecurse =
            List.any expessionIncludesApplicationNotBehindLambda
    in
    case Node.value expr of
        -- ----------------------------------------------------
        -- Immediately False
        -- ----------------------------------------------------
        UnitExpr ->
            False

        FunctionOrValue _ _ ->
            False

        PrefixOperator _ ->
            False

        Operator _ ->
            False

        Integer _ ->
            False

        Hex _ ->
            False

        Floatable _ ->
            False

        Literal _ ->
            False

        CharLiteral _ ->
            False

        RecordAccessFunction _ ->
            False

        GLSLExpression _ ->
            False

        LambdaExpression _ ->
            False

        -- ----------------------------------------------------
        -- Immediately True
        -- ----------------------------------------------------
        Application _ ->
            True

        -- ----------------------------------------------------
        -- Recurse
        -- ----------------------------------------------------
        Negation inner ->
            expessionIncludesApplicationNotBehindLambda inner

        OperatorApplication _ _ l r ->
            expressionsRecurse [ l, r ]

        IfBlock c t f ->
            expressionsRecurse [ c, t, f ]

        TupledExpression exprs ->
            expressionsRecurse exprs

        ParenthesizedExpression e ->
            expressionsRecurse [ e ]

        LetExpression { declarations, expression } ->
            (declarations |> List.any letDeclaration) || expessionIncludesApplicationNotBehindLambda expression

        CaseExpression { cases, expression } ->
            expressionsRecurse (expression :: (cases |> List.map Tuple.second))

        RecordExpr setters ->
            setters
                |> List.map (Node.value >> Tuple.second)
                |> expressionsRecurse

        RecordUpdateExpression _ setters ->
            setters
                |> List.map (Node.value >> Tuple.second)
                |> expressionsRecurse

        ListExpr exprs ->
            expressionsRecurse exprs

        RecordAccess e _ ->
            expessionIncludesApplicationNotBehindLambda e


errorsInExpression : LambdaReduceStrategy -> Set.Set String -> Errors -> Node Expression -> Errors
errorsInExpression lambdaReduceStrategy valueNameSet initialErrors =
    let
        letDeclaration : Node LetDeclaration -> Errors -> Errors
        letDeclaration decl accumulatedErrors =
            case Node.value decl of
                LetFunction { declaration } ->
                    errorsInFunctionImplementation lambdaReduceStrategy valueNameSet declaration accumulatedErrors

                LetDestructuring _ expr ->
                    expressionRecurse accumulatedErrors expr

        expressionRecurse : Errors -> Node Expression -> Errors
        expressionRecurse accumulatedErrors expr =
            let
                expressionsRecurse =
                    List.foldl (Ra.flip expressionRecurse) accumulatedErrors
            in
            case Node.value expr of
                -- ------------------------------------------------------
                -- Return Immediately
                -- ------------------------------------------------------
                UnitExpr ->
                    accumulatedErrors

                FunctionOrValue _ _ ->
                    accumulatedErrors

                PrefixOperator _ ->
                    accumulatedErrors

                Operator _ ->
                    accumulatedErrors

                Integer _ ->
                    accumulatedErrors

                Hex _ ->
                    accumulatedErrors

                Floatable _ ->
                    accumulatedErrors

                Negation inner ->
                    expressionRecurse accumulatedErrors inner

                Literal _ ->
                    accumulatedErrors

                CharLiteral _ ->
                    accumulatedErrors

                RecordAccessFunction _ ->
                    accumulatedErrors

                GLSLExpression _ ->
                    accumulatedErrors

                -- ------------------------------------------------------
                -- Recurse Only
                -- ------------------------------------------------------
                Application exprs ->
                    expressionsRecurse exprs

                OperatorApplication _ _ l r ->
                    expressionsRecurse [ l, r ]

                IfBlock c t f ->
                    expressionsRecurse [ c, t, f ]

                TupledExpression exprs ->
                    expressionsRecurse exprs

                ParenthesizedExpression e ->
                    expressionsRecurse [ e ]

                LetExpression { declarations, expression } ->
                    expressionRecurse (declarations |> List.foldl letDeclaration accumulatedErrors) expression

                CaseExpression { cases, expression } ->
                    expressionsRecurse (expression :: (cases |> List.map Tuple.second))

                RecordExpr setters ->
                    setters
                        |> List.map (Node.value >> Tuple.second)
                        |> expressionsRecurse

                RecordUpdateExpression _ setters ->
                    setters
                        |> List.map (Node.value >> Tuple.second)
                        |> expressionsRecurse

                ListExpr exprs ->
                    expressionsRecurse exprs

                RecordAccess e _ ->
                    expressionRecurse accumulatedErrors e

                -- ------------------------------------------------------
                -- Actually Do the Thing!!!
                -- ------------------------------------------------------
                LambdaExpression { args, expression } ->
                    expressionRecurse
                        (expression
                            |> Node.value
                            |> functionApplication
                            |> Maybe.map
                                (\applications ->
                                    errorsInApplicationOfLambda
                                        lambdaReduceStrategy
                                        valueNameSet
                                        (Node.range expr)
                                        accumulatedErrors
                                        args
                                        applications
                                )
                            |> Maybe.withDefault accumulatedErrors
                        )
                        expression
    in
    expressionRecurse initialErrors


errorsInFunctionImplementation : LambdaReduceStrategy -> Set String -> Node FunctionImplementation -> Errors -> Errors
errorsInFunctionImplementation lambdaReduceStrategy valueNameSet declaration accumulatedErrors =
    let
        { name, expression, arguments } =
            Node.value declaration
    in
    errorsInExpression
        lambdaReduceStrategy
        ((if List.isEmpty arguments then
            name |> Node.value |> Set.insert

          else
            identity
         )
            valueNameSet
        )
        accumulatedErrors
        expression


errorsInDeclaration : LambdaReduceStrategy -> Node Declaration -> Errors
errorsInDeclaration lambdaReduceStrategy (Node _ decl) =
    case decl of
        CustomTypeDeclaration _ ->
            []

        FunctionDeclaration { declaration } ->
            errorsInFunctionImplementation lambdaReduceStrategy Set.empty declaration []

        AliasDeclaration _ ->
            []

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []
