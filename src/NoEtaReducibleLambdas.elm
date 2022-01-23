module NoEtaReducibleLambdas exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Ra
import Review.Fix exposing (Fix)
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


varPattern : Pattern -> Maybe String
varPattern p =
    case p of
        VarPattern v ->
            Just v

        _ ->
            Nothing


errorsInExpression : String -> Errors -> Node Expression -> Errors
errorsInExpression functionName initialErrors =
    let
        errorsInApplicationOfLambda : Range -> Errors -> List Pattern -> List Expression -> List (Rule.Error {})
        errorsInApplicationOfLambda range acc arguments expressions =
            let
                variableCounts =
                    VariableName.countInExpressions expressions Dict.empty

                countOf name =
                    Dict.get name variableCounts
                        |> Maybe.withDefault 0

                ( canBeRemoved, mustBeRetained ) =
                    List.map2
                        Tuple.pair
                        (List.reverse arguments)
                        (List.reverse expressions)
                        |> Ra.partitionWhile
                            (\( argumentPattern, expression ) ->
                                Maybe.map2
                                    (\argumentName variableName -> argumentName == variableName && countOf variableName <= 1)
                                    (varPattern argumentPattern)
                                    (VariableName.fromExpression expression)
                                    |> Maybe.withDefault False
                            )

                mustKeepLambda =
                    expressions
                        |> List.head
                        |> Maybe.andThen VariableName.fromExpression
                        |> Maybe.map ((==) functionName)
                        |> Maybe.withDefault False
            in
            if List.isEmpty canBeRemoved then
                acc

            else if List.isEmpty mustBeRetained && not mustKeepLambda then
                -- we can reduce all the way down to just the function expression
                Rule.errorWithFix { message = "You can reduce to just a function", details = [ "x" ] } range [] :: acc

            else
                -- we can reduce many of the arguments
                Rule.errorWithFix { message = "You can reduce", details = [ "x" ] } range [] :: acc

        letDeclaration : Node LetDeclaration -> Errors -> Errors
        letDeclaration decl errorsSoFar =
            case Node.value decl of
                LetFunction { declaration } ->
                    errorsInFunctionImplementation declaration errorsSoFar

                LetDestructuring _ expr ->
                    expressionRecurse errorsSoFar expr

        expressionRecurse acc expr =
            let
                expressionsRecurse =
                    List.foldl (Ra.flip expressionRecurse) acc
            in
            case Node.value expr of
                -- ------------------------------------------------------
                -- Return Immediately
                -- ------------------------------------------------------
                UnitExpr ->
                    acc

                FunctionOrValue _ _ ->
                    acc

                PrefixOperator _ ->
                    acc

                Operator _ ->
                    acc

                Integer _ ->
                    acc

                Hex _ ->
                    acc

                Floatable _ ->
                    acc

                Negation _ ->
                    acc

                Literal _ ->
                    acc

                CharLiteral _ ->
                    acc

                RecordAccessFunction _ ->
                    acc

                GLSLExpression _ ->
                    acc

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
                    expressionRecurse (declarations |> List.foldl letDeclaration acc) expression

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
                    expressionRecurse acc e

                -- ------------------------------------------------------
                -- Actually Do the Thing!!!
                -- ------------------------------------------------------
                LambdaExpression { args, expression } ->
                    expressionRecurse
                        (expression
                            |> Node.value
                            |> functionApplication
                            |> Maybe.map (\applications -> errorsInApplicationOfLambda (Node.range expr) acc (nodeValues args) (applications |> nodeValues))
                            |> Maybe.withDefault acc
                        )
                        expression
    in
    expressionRecurse initialErrors


errorsInFunctionImplementation : Node FunctionImplementation -> Errors -> Errors
errorsInFunctionImplementation declaration errors =
    let
        { name, expression } =
            Node.value declaration
    in
    errorsInExpression (Node.value name) errors expression


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
