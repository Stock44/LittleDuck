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

    /// <summary>
    /// This function uses a reader monad to access the semantic context in which the CTE type is resolved.
    /// The function differentiates between different types of CTEs such as NegativeCTE, PositiveCTE, ValueCTE, and IdentifierCTE,
    /// and returns the appropriate resolved value type based on the CTE type.
    /// </summary>
    ///
    /// <param name="cte">A CTENode representing the CTE whose type needs to be resolved.</param>
    ///
    /// <returns>
    /// A ReaderT monad over the SemanticContext, encapsulating the resolved type, or an error if the node is invalid.
    /// </returns>
    let rec cteType (cte: CTENode) : ReaderT<SemanticContext, _> =
        monad' {
            let! ctx = Reader.ask |> ReaderT.hoist

            match cte with
            | NegativeCTE valueNode -> return valueType valueNode
            | PositiveCTE valueNode -> return valueType valueNode
            | ValueCTE valueNode -> return valueType valueNode
            | IdentifierCTE(IdentifierNode name) ->
                match!
                    name
                    |> SemanticContext.getSymbol ctx
                    |> first NonEmptyList.singleton
                    |> ReaderT.lift
                with
                | Program -> return! ProgramUsedAsValue |> NonEmptyList.singleton |> Error |> ReaderT.lift
                | Function _ -> return! FunctionUsedAsValue name |> NonEmptyList.singleton |> Error |> ReaderT.lift
                | Argument ty -> return ty
                | Variable ty -> return ty
            | InvocationCTE(InvocationNode(IdentifierNode name, _)) ->
                match!
                    name
                    |> SemanticContext.getSymbol ctx
                    |> first NonEmptyList.singleton
                    |> ReaderT.lift
                with
                | Program -> return! InvokedProgram |> NonEmptyList.singleton |> Error |> ReaderT.lift
                | Argument _ -> return! InvokedArgument name |> NonEmptyList.singleton |> Error |> ReaderT.lift
                | Variable _ -> return! InvokedVariable name |> NonEmptyList.singleton |> Error |> ReaderT.lift
                | Function(FunctionDeclarationNode(Void, _, _, _, _)) ->
                    return! VoidFunctionUsedAsValue name |> NonEmptyList.singleton |> Error |> ReaderT.lift
                | Function(FunctionDeclarationNode(ReturnType ty, _, _, _, _)) -> return ty
        }

    /// <summary>
    /// Resolves the type of a given factor node by pattern matching against the
    /// type of factor and delegating the type resolution accordingly.
    /// </summary>
    ///
    /// <param name="factor"> The factor node whose type needs to be resolved. </param>
    ///
    /// <returns>
    /// The type of the factor node, wrapped in a ReaderT Result that can resolve to an error if the type resolution failed.
    /// </returns>
    and factorType (factor: FactorNode) =
        match factor with
        | CTEFactor cte -> cteType cte
        | ParenthesizedExprFactor expressionNode -> expressionType expressionNode

    /// <summary>
    /// Resolves the type of a given term node by pattern matching against the
    /// type of term and delegating the type resolution accordingly.
    /// </summary>
    ///
    /// <param name="term">The term node whose type needs to be resolved.</param>
    ///
    /// <returns>
    /// The type of the term node, wrapped in a ReaderT Result that can resolve to an error if the type resolution failed.
    /// </returns>
    and termType (term: TermNode) =
        match term with
        | FactorTerm factorNode -> factorType factorNode
        | DivisionTerm(termNode, factorNode) ->
            monad' {
                let! lhsType = termType termNode
                and! rhsType = factorType factorNode

                match lhsType, rhsType with
                | IntType, IntType -> return IntType
                | IntType, FloatType -> return FloatType
                | FloatType, IntType -> return FloatType
                | FloatType, FloatType -> return FloatType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("/", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }
        | MultiplicationTerm(termNode, factorNode) ->
            monad' {
                let! lhsType = termType termNode
                and! rhsType = factorType factorNode

                match lhsType, rhsType with
                | IntType, IntType -> return IntType
                | IntType, FloatType -> return FloatType
                | FloatType, IntType -> return FloatType
                | FloatType, FloatType -> return FloatType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("*", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }

    /// <summary>
    /// Analyzes the provided Exp and determines its type within
    /// the context of the current semantic environment.
    /// It ensures type correctness
    /// and helps identify any type-related errors.
    /// </summary>
    ///
    /// <param name="exp">
    ///         The Exp whose type needs to be resolved.
    /// </param>
    ///
    /// <returns>
    /// The type of the Exp node,
    /// wrapped in a ReaderT Result that can resolve to an error if the type resolution failed.
    /// </returns>
    and expType (exp: ExpNode) =
        match exp with
        | TermExp termNode -> termType termNode
        | SumExp(expNode, termNode) ->
            monad' {
                let! lhsType = expType expNode
                and! rhsType = termType termNode

                match lhsType, rhsType with
                | IntType, IntType -> return IntType
                | IntType, FloatType -> return FloatType
                | FloatType, IntType -> return FloatType
                | FloatType, FloatType -> return FloatType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("+", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }
        | SubtractionExp(expNode, termNode) ->
            monad' {
                let! lhsType = expType expNode
                and! rhsType = termType termNode

                match lhsType, rhsType with
                | IntType, IntType -> return IntType
                | IntType, FloatType -> return FloatType
                | FloatType, IntType -> return FloatType
                | FloatType, FloatType -> return FloatType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("-", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }

    /// Resolves the type of a given expression in the context of the current semantic analysis.
    ///
    /// This function determines the type of the provided expression based on the context within which it appears,
    /// ensuring that types are correctly inferred or validated according to the rules of the programming language
    /// semantics being analyzed.
    ///
    /// expression: The expression whose type needs to be resolved.
    /// context: The current context of the semantic analysis, which may include type environments, variable scopes, etc.
    ///
    /// returns: The resolved type of the given expression.
    and expressionType (expression: ExpressionNode) =
        match expression with
        | LessThanExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                match lhsType, rhsType with
                | IntType, IntType -> return BooleanType
                | IntType, FloatType -> return BooleanType
                | FloatType, IntType -> return BooleanType
                | FloatType, FloatType -> return BooleanType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("<", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }
        | MoreThanExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                match lhsType, rhsType with
                | IntType, IntType -> return BooleanType
                | IntType, FloatType -> return BooleanType
                | FloatType, IntType -> return BooleanType
                | FloatType, FloatType -> return BooleanType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes(">", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }
        | NotEqualExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                match lhsType, rhsType with
                | IntType, IntType -> return BooleanType
                | IntType, FloatType -> return BooleanType
                | FloatType, IntType -> return BooleanType
                | FloatType, FloatType -> return BooleanType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("!=", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }
        | EqualExpression(lhs, rhs) ->
            monad' {
                let! lhsType = expType lhs
                and! rhsType = expType rhs

                match lhsType, rhsType with
                | IntType, IntType -> return BooleanType
                | IntType, FloatType -> return BooleanType
                | FloatType, IntType -> return BooleanType
                | FloatType, FloatType -> return BooleanType
                | _ ->
                    return!
                        InvalidBinaryOperandTypes("==", lhsType, rhsType)
                        |> NonEmptyList.singleton
                        |> Error
                        |> ReaderT.lift
            }
        | ExpExpression expNode -> expType expNode
