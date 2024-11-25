[<AutoOpen>]
module LittleDuck.TypeResolvers

open FSharpPlus
open FSharpPlus.Data

[<RequireQualifiedAccess>]
module Resolve =
    /// Resolves the type of a value node.
    let valueType (value: ValueNode) =
        match value with
        | Float _ -> FloatType
        | Int _ -> IntType
        | String _ -> StringType
        | Boolean _ -> BooleanType

    let rec cteType (cte: CTENode) : ReaderT<SemanticContext, TypeNode option> =
        monad' {
            let! ctx = Reader.ask |> ReaderT.hoist

            match cte with
            | NegativeCTE valueNode -> valueNode |> valueType
            | PositiveCTE valueNode -> valueNode |> valueType
            | ValueCTE valueNode -> valueNode |> valueType
            | IdentifierCTE(IdentifierNode name) ->
                match! name |> SemanticContext.tryGetSymbol ctx |> ReaderT.lift with
                | Program -> return! None |> ReaderT.lift
                | Function _ -> return! None |> ReaderT.lift
                | Argument ty -> ty
                | Variable ty -> ty
            | InvocationCTE(InvocationNode(IdentifierNode name, _)) ->
                match! name |> SemanticContext.tryGetSymbol ctx |> ReaderT.lift with
                | Program -> return! None |> ReaderT.lift
                | Argument _ -> return! None |> ReaderT.lift
                | Variable _ -> return! None |> ReaderT.lift
                | Function(FunctionDeclarationNode(Void, _, _, _, _)) -> return! None |> ReaderT.lift
                | Function(FunctionDeclarationNode(ReturnType ty, _, _, _, _)) -> ty
        }

    and factorType (factor: FactorNode) =
        match factor with
        | CTEFactor cte -> cteType cte
        | ParenthesizedExprFactor expressionNode -> expressionType expressionNode

    and termType (term: TermNode) =
        match term with
        | FactorTerm factorNode -> factorType factorNode
        | DivisionTerm(termNode, factorNode) ->
            monad' {
                let! lhsType = termType termNode
                and! rhsType = factorType factorNode

                return! validateDivision lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }
        | MultiplicationTerm(termNode, factorNode) ->
            monad' {
                let! lhsType = termType termNode
                and! rhsType = factorType factorNode

                return! validateMultiplication lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }

    and expType (exp: ExpNode) =
        match exp with
        | TermExp termNode -> termType termNode
        | SumExp(expNode, termNode) ->
            monad' {
                let! lhsType = expType expNode
                and! rhsType = termType termNode

                return! validateAddition lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }
        | SubtractionExp(expNode, termNode) ->
            monad' {
                let! lhsType = expType expNode
                and! rhsType = termType termNode

                return! validateSubtraction lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }

    and expressionType (expression: ExpressionNode) =
        match expression with
        | LessThanExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                return! validateLessThan lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }
        | GreaterThanExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                return! validateGreaterThan lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }
        | NotEqualExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                return! validateNotEqual lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }
        | EqualsExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                return! validateNotEqual lhsType rhsType |> Option.ofResult |> ReaderT.lift
            }
        | ExpExpression expNode -> expType expNode
