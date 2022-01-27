module VariableName exposing (countInExpression, countInExpressions, countInLetDeclaration, fromExpression)

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Node.Extra


fromExpression : Node Expression -> Maybe (Node String)
fromExpression (Node range e) =
    case e of
        FunctionOrValue [] name ->
            Just (Node range name)

        _ ->
            Nothing


countInLetDeclaration : LetDeclaration -> Dict String Int
countInLetDeclaration decl =
    case decl of
        LetFunction { declaration } ->
            declaration
                |> Node.value
                |> .expression
                |> Node.value
                |> countInExpression

        LetDestructuring _ expr ->
            expr |> Node.value |> countInExpression


unionCount : Dict comparable Int -> Dict comparable Int -> Dict comparable Int
unionCount l r =
    let
        inBoth key left right =
            Dict.insert key (left + right)
    in
    Dict.merge Dict.insert inBoth Dict.insert l r Dict.empty


countInExpressions : List Expression -> Dict String Int -> Dict String Int
countInExpressions expressions dict =
    expressions
        |> List.map countInExpression
        |> List.foldl unionCount dict


countInExpression : Expression -> Dict String Int
countInExpression =
    let
        aux acc expr =
            let
                expressions exprs =
                    countInExpressions (exprs |> Node.Extra.values) acc
            in
            case expr of
                FunctionOrValue [] name ->
                    Dict.update
                        name
                        (\maybeCount ->
                            case maybeCount of
                                Nothing ->
                                    Just 1

                                Just existing ->
                                    Just (existing + 1)
                        )
                        acc

                FunctionOrValue _ _ ->
                    acc

                UnitExpr ->
                    acc

                Application exprs ->
                    expressions exprs

                OperatorApplication _ _ left right ->
                    expressions [ left, right ]

                IfBlock cond true false ->
                    expressions [ cond, true, false ]

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

                Negation e ->
                    expressions [ e ]

                Literal _ ->
                    acc

                CharLiteral _ ->
                    acc

                TupledExpression exprs ->
                    expressions exprs

                ParenthesizedExpression e ->
                    expressions [ e ]

                LetExpression { declarations, expression } ->
                    declarations
                        |> Node.Extra.values
                        |> List.map countInLetDeclaration
                        |> (::) (expressions [ expression ])
                        |> List.foldl unionCount acc

                CaseExpression { cases, expression } ->
                    cases
                        |> List.map Tuple.second
                        |> (::) expression
                        |> expressions

                LambdaExpression { expression } ->
                    expressions [ expression ]

                RecordExpr recordSetters ->
                    recordSetters
                        |> List.map (Node.value >> Tuple.second)
                        |> expressions

                ListExpr a ->
                    expressions a

                RecordAccess recordExpr _ ->
                    expressions [ recordExpr ]

                RecordAccessFunction _ ->
                    acc

                RecordUpdateExpression _ recordSetters ->
                    recordSetters
                        |> List.map (Node.value >> Tuple.second)
                        |> expressions

                GLSLExpression _ ->
                    acc
    in
    aux Dict.empty
