[<AutoOpen>]
module LittleDuck.ErrorCollectors

open FSharpPlus
open FSharpPlus.Data

[<AutoOpen>]
module Helpers =
    let getSymbol x =
        Reader.ask |>> fun r -> SemanticContext.getSymbol r x

    let inline switchOption (x: ReaderT<'a, 'b option>) : ReaderT<'a, 'b option seq> =
        Reader.ask |>> (fun r -> ReaderT.run x r) |> ReaderT.hoist

    let resolveOpValidation x =
        match x with
        | Some(Error error) -> error |> Seq.singleton |> ReaderT.lift
        | None -> UnresolvedType |> Seq.singleton |> ReaderT.lift
        | _ -> Seq.empty |> ReaderT.lift

    let getReturnType = Reader.ask |>> _.ReturnType

    let getCurrentName = Reader.ask |>> _.Name

[<RequireQualifiedAccess>]
module Collect =
    let rec cteErrors (cte: CTENode) : ReaderT<SemanticContext, _ seq> =
        monad.plus {
            match cte with
            | NegativeCTE _
            | PositiveCTE _
            | ValueCTE _ -> ()
            | IdentifierCTE(IdentifierNode name) ->
                match! name |> getSymbol |> ReaderT.hoist with
                | Ok(Program) -> yield ProgramUsedAsValue
                | Ok(Function _) -> yield FunctionUsedAsValue name
                | Ok(Argument _) -> ()
                | Ok(Variable _) -> ()
                | Error error -> yield error
            | InvocationCTE(InvocationNode(IdentifierNode name, _) as invocation) ->
                match! name |> getSymbol |> ReaderT.hoist with
                | Ok(Program) -> yield InvokedProgram
                | Ok(Argument _) -> yield InvokedArgument name
                | Ok(Variable _) -> yield InvokedVariable name
                | Ok(Function(FunctionDeclarationNode(Void, _, _, _, _))) ->
                    yield VoidFunctionUsedAsValue name
                    yield! invocationErrors invocation
                | Ok(Function(FunctionDeclarationNode _)) -> yield! invocationErrors invocation
                | Error error -> yield error
        }

    and factorErrors (factor: FactorNode) =
        match factor with
        | CTEFactor cteNode -> cteErrors cteNode
        | ParenthesizedExprFactor expressionNode -> expressionErrors expressionNode

    and termErrors (term: TermNode) : ReaderT<SemanticContext, _> =
        monad.plus {
            match term with
            | FactorTerm factorNode -> return! factorErrors factorNode
            | DivisionTerm(termNode, factorNode) ->
                yield! termErrors termNode
                yield! factorErrors factorNode

                let! termType = termNode |> Resolve.termType |> switchOption
                let! factorType = factorNode |> Resolve.factorType |> switchOption
                yield! validateDivision <!> termType <*> factorType |> resolveOpValidation
            | MultiplicationTerm(termNode, factorNode) ->
                yield! termErrors termNode
                yield! factorErrors factorNode

                let! termType = termNode |> Resolve.termType |> switchOption
                let! factorType = factorNode |> Resolve.factorType |> switchOption
                yield! validateMultiplication <!> termType <*> factorType |> resolveOpValidation
        }

    and expErrors (exp: ExpNode) =
        monad.plus {
            match exp with
            | TermExp termNode -> return! termErrors termNode
            | SumExp(expNode, termNode) ->
                yield! expErrors expNode
                yield! termErrors termNode

                let! expType = expNode |> Resolve.expType |> switchOption
                let! termType = termNode |> Resolve.termType |> switchOption
                yield! validateAddition <!> expType <*> termType |> resolveOpValidation
            | SubtractionExp(expNode, termNode) ->
                yield! expErrors expNode
                yield! termErrors termNode

                let! expType = expNode |> Resolve.expType |> switchOption
                let! termType = termNode |> Resolve.termType |> switchOption
                yield! validateSubtraction <!> expType <*> termType |> resolveOpValidation
        }

    and expressionErrors (expression: ExpressionNode) =
        monad.plus {
            match expression with
            | ExpExpression expNode -> return! expErrors expNode
            | LessThanExpression(lhs, rhs) ->
                yield! expErrors lhs
                yield! expErrors rhs

                let! lhsType = lhs |> Resolve.expType |> switchOption
                let! rhsType = rhs |> Resolve.expType |> switchOption
                yield! validateLessThan <!> lhsType <*> rhsType |> resolveOpValidation
            | GreaterThanExpression(lhs, rhs) ->
                yield! expErrors lhs
                yield! expErrors rhs

                let! lhsType = lhs |> Resolve.expType |> switchOption
                let! rhsType = rhs |> Resolve.expType |> switchOption
                yield! validateGreaterThan <!> lhsType <*> rhsType |> resolveOpValidation
            | NotEqualExpression(lhs, rhs) ->
                yield! expErrors lhs
                yield! expErrors rhs

                let! lhsType = lhs |> Resolve.expType |> switchOption
                let! rhsType = rhs |> Resolve.expType |> switchOption
                yield! validateNotEqual <!> lhsType <*> rhsType |> resolveOpValidation
            | EqualsExpression(lhs, rhs) ->
                yield! expErrors lhs
                yield! expErrors rhs

                let! lhsType = lhs |> Resolve.expType |> switchOption
                let! rhsType = rhs |> Resolve.expType |> switchOption
                yield! validateEquals <!> lhsType <*> rhsType |> resolveOpValidation
        }

    and invocationErrors (InvocationNode(IdentifierNode functionName, arguments)) =
        monad.plus {
            match! functionName |> getSymbol |> ReaderT.hoist with
            | Error error -> yield error
            | Ok(Variable _) -> yield InvokedVariable functionName
            | Ok(Argument _) -> yield InvokedArgument functionName
            | Ok Program -> yield InvokedProgram
            | Ok(Function(FunctionDeclarationNode(_, _, funArgs, _, _))) ->
                // Get the count of the arguments
                let receivedArgCount = arguments.Length
                let expectedArgCount = funArgs.Length

                if receivedArgCount <> expectedArgCount then
                    yield InvocationArgumentCountMismatch(functionName, receivedArgCount, expectedArgCount)
                else
                    for expression, ArgumentNode(IdentifierNode argumentName, argumentType) in
                        List.zip arguments funArgs do
                        // Check the expression for errors
                        yield! expressionErrors expression

                        // Resolve the expression type
                        match! expression |> Resolve.expressionType |> switchOption with
                        | None -> yield UnresolvedType
                        | Some exprType ->
                            if areTypesCompatible exprType argumentType then
                                ()
                            else
                                yield InvocationArgumentTypeMismatch(functionName, argumentName, exprType, argumentType)
        }


    let assignmentErrors (AssignmentNode(IdentifierNode(name), expression)) =
        monad.plus {
            yield! expressionErrors expression

            match! name |> getSymbol |> ReaderT.hoist with
            | Ok(Variable variableType) ->
                match! expression |> Resolve.expressionType |> switchOption with
                | None -> yield UnresolvedType
                | Some expressionType ->
                    if areTypesCompatible expressionType variableType then
                        ()
                    else
                        yield AssignmentWithWrongType(name, variableType, expressionType)
            | Ok(Function _) -> yield AssignmentToFunction(name)
            | Ok(Program) -> yield AssignmentToProgram
            | Ok(Argument _) -> yield AssignmentToArgument(name)
            | Error(error) -> yield error

        }


    let rec conditionErrors (node: ConditionNode) =
        monad.plus {
            match node with
            | IfElseCondition(IfNode(ifExpr, ifBody), ElseNode(elseExpr, elseBody)) ->
                yield! expressionErrors ifExpr
                yield! bodyErrors ifBody
                yield! expressionErrors elseExpr
                yield! bodyErrors elseBody

                let! ifExprType = ifExpr |> Resolve.expressionType |> switchOption

                let! elseExprType = elseExpr |> Resolve.expressionType |> switchOption

                match ifExprType, elseExprType with
                | Some(ifExprType), Some(elseExprType) ->
                    match validateCondition ifExprType with
                    | Some error -> yield error
                    | _ -> ()

                    match validateCondition elseExprType with
                    | Some error -> yield error
                    | _ -> ()
                | None, Some _
                | Some _, None -> yield UnresolvedType
                | None, None ->
                    yield UnresolvedType
                    yield UnresolvedType
            | IfCondition(IfNode(ifExpr, ifBody)) ->
                yield! expressionErrors ifExpr
                yield! bodyErrors ifBody

                let! ifExprType = ifExpr |> Resolve.expressionType |> switchOption

                match ifExprType with
                | Some(ifExprType) ->
                    match validateCondition ifExprType with
                    | Some error -> yield error
                    | _ -> ()
                | _ -> yield UnresolvedType
        }

    and cycleErrors (CycleNode(body, expression)) =
        monad.plus {
            yield! bodyErrors body
            yield! expressionErrors expression

            match! expression |> Resolve.expressionType |> switchOption with
            | Some BooleanType -> ()
            | Some ty -> yield InvalidConditionType ty
            | None -> yield UnresolvedType
        }


    and printErrors (PrintNode expressions) =
        monad.plus {
            for expression in expressions do
                yield! expressionErrors expression

                match! expression |> Resolve.expressionType |> switchOption with
                | None -> yield UnresolvedType
                | _ -> ()
        }

    and returnErrors (ReturnNode optExpression) =
        monad.plus {
            let! returnType = getReturnType |> ReaderT.hoist
            let! name = getCurrentName |> ReaderT.hoist

            match returnType, optExpression with
            | Void, None -> ()
            | Void, Some _ -> yield ReturnedValueFromVoidFunction name
            | ReturnType returnType, None -> yield ReturnedNothingFromValueReturningFunction(name, returnType)
            | ReturnType returnType, Some(expression) ->
                yield! expressionErrors expression

                match! expression |> Resolve.expressionType |> switchOption with
                | None -> yield UnresolvedType
                | Some expressionType ->
                    if areTypesCompatible expressionType returnType then
                        ()
                    else
                        yield ReturnTypeMismatch(name, expressionType, returnType)
        }

    and bodyErrors (BodyNode body) =
        monad.plus {
            let! statement = body |> ReaderT.lift

            match statement with
            | AssignmentStatement node -> return! assignmentErrors node
            | ConditionStatement node -> return! conditionErrors node
            | CycleStatement node -> return! cycleErrors node
            | InvocationStatement node -> return! invocationErrors node
            | PrintStatement node -> return! printErrors node
            | ReturnStatement node -> return! returnErrors node
        }
