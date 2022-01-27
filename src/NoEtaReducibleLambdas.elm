module NoEtaReducibleLambdas exposing
    ( rule
    , canReduceFunctionArguments, canReduceToJustFunctionApplication
    )

{-|

@docs rule

-}

-- import Review.Fix exposing (Fix)

import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Ra
import Range.Extra exposing (expandToLeft, expandToRight, toTheLeft)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import VariableName


{-| Reports... REPLACEME

    config =
        [ NoEtaReducibleLambdas.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jsuder-xx/elm-review-eta-reduction/example --rules NoEtaReducibleLambdas
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoEtaReducibleLambdas" ()
        |> Rule.withDeclarationEnterVisitor (\decl context -> ( errorsInDeclaration decl, context ))
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


canReduceToJustFunctionApplication : { message : String, details : List String }
canReduceToJustFunctionApplication =
    { message = "This lambda can be replaced entirely with a function application."
    , details =
        [ "This lambda can be refactored to just the function itself through identity."
        ]
    }


canReduceFunctionArguments : { message : String, details : List String }
canReduceFunctionArguments =
    { message = "Arguments can be removed from this lambda and the function application."
    , details = [ "Arguments can be removed." ]
    }


removeWithLeadingSpace : Range -> Fix.Fix
removeWithLeadingSpace =
    expandToLeft 1 >> Fix.removeRange


removeParameterAndExpressionFixes : ( Node a, Node b ) -> List Fix.Fix
removeParameterAndExpressionFixes ( Node patternRange _, Node expressionRange _ ) =
    [ removeWithLeadingSpace patternRange
    , removeWithLeadingSpace expressionRange
    ]


errorsInApplicationOfLambda : String -> Range -> Errors -> List (Node Pattern) -> List (Node Expression) -> List (Rule.Error {})
errorsInApplicationOfLambda inDeclaredFunctionName range existingErrors arguments expressions =
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

                mustKeepLambda =
                    functionExpression
                        |> VariableName.fromExpression
                        |> Maybe.map Node.value
                        |> Maybe.map ((==) inDeclaredFunctionName)
                        |> Maybe.withDefault False

                ( canBeRemoved, mustBeRetained ) =
                    if mustKeepLambda && List.isEmpty retainable then
                        case List.reverse removable of
                            [] ->
                                ( removable, retainable )

                            last :: rest ->
                                ( List.reverse rest, [ last ] )

                    else
                        ( removable, retainable )
            in
            if List.isEmpty canBeRemoved then
                existingErrors

            else if List.isEmpty mustBeRetained then
                Rule.errorWithFix
                    canReduceToJustFunctionApplication
                    range
                    (Fix.removeRange (functionExpression |> Node.range |> toTheLeft { delta = 0, count = 4 })
                        :: List.concatMap
                            removeParameterAndExpressionFixes
                            canBeRemoved
                    )
                    :: existingErrors

            else
                Rule.errorWithFix
                    canReduceFunctionArguments
                    range
                    (List.concatMap
                        removeParameterAndExpressionFixes
                        canBeRemoved
                    )
                    :: existingErrors


errorsInExpression : String -> Errors -> Node Expression -> Errors
errorsInExpression functionName initialErrors =
    let
        letDeclaration : Node LetDeclaration -> Errors -> Errors
        letDeclaration decl accumulatedErrors =
            case Node.value decl of
                LetFunction { declaration } ->
                    errorsInFunctionImplementation declaration accumulatedErrors

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

                Negation _ ->
                    accumulatedErrors

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
                                        functionName
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


errorsInFunctionImplementation : Node FunctionImplementation -> Errors -> Errors
errorsInFunctionImplementation declaration accumulatedErrors =
    let
        { name, expression } =
            Node.value declaration
    in
    errorsInExpression (Node.value name) accumulatedErrors expression


errorsInDeclaration : Node Declaration -> Errors
errorsInDeclaration (Node _ decl) =
    case decl of
        CustomTypeDeclaration _ ->
            []

        FunctionDeclaration { declaration } ->
            errorsInFunctionImplementation declaration []

        AliasDeclaration _ ->
            []

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []
