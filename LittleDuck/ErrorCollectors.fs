[<AutoOpen>]
module LittleDuck.ErrorCollectors

open FSharpPlus
open FSharpPlus.Data


[<RequireQualifiedAccess>]
module Collect =
    let rec cteErrors (cte: CTENode) =
        monad' {
            match cte with
            | NegativeCTE _
            | PositiveCTE _
            | ValueCTE _
            | IdentifierCTE _ -> return Seq.empty
            | InvocationCTE invocationNode -> return! invocationArgumentErrors invocationNode
        }

    and factorErrors (factor: FactorNode) =
        monad' {
            match factor with
            | CTEFactor cteNode -> return! cteErrors cteNode
            | ParenthesizedExprFactor expressionNode -> return! expressionErrors expressionNode
        }

    and termErrors (term: TermNode) =
        monad' {
            match term with
            | FactorTerm factorNode -> return! factorErrors factorNode
            | DivisionTerm(termNode, factorNode) ->
                return! Seq.append <!> termErrors termNode <*> factorErrors factorNode
            | MultiplicationTerm(termNode, factorNode) ->
                return! Seq.append <!> termErrors termNode <*> factorErrors factorNode
        }

    and expErrors (exp: ExpNode) =
        monad' {
            match exp with
            | TermExp termNode -> return! termErrors termNode
            | SumExp(expNode, termNode) -> return! Seq.append <!> expErrors expNode <*> termErrors termNode
            | SubtractionExp(expNode, termNode) -> return! Seq.append <!> expErrors expNode <*> termErrors termNode
        }

    and expressionErrors (expression: ExpressionNode) =
        monad' {
            match expression with
            | ExpExpression expNode -> return! expErrors expNode
            | LessThanExpression(expNode, node) -> return! Seq.append <!> expErrors expNode <*> expErrors node
            | MoreThanExpression(expNode, node) -> return! Seq.append <!> expErrors expNode <*> expErrors node
            | NotEqualExpression(expNode, node) -> return! Seq.append <!> expErrors expNode <*> expErrors node
            | EqualExpression(expNode, node) -> return! Seq.append <!> expErrors expNode <*> expErrors node
        }

    and invocationArgumentErrors (InvocationNode(IdentifierNode functionName, arguments)) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx
            let runT = fun x -> ReaderT.run x ctx

            let symbol = functionName |> SemanticContext.getSymbol ctx

            seq {
                match symbol with
                | Error _ -> () // We are not reporting resolution errors here
                | Ok(Variable _) -> ()
                | Ok(Argument _) -> ()
                | Ok Program -> ()
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
                            yield! run (expressionErrors expression)

                            // Resolve the expression type
                            match runT (Resolve.expressionType expression) with
                            | Error errors -> yield! errors
                            | Ok exprType ->
                                if exprType = argumentType then
                                    ()
                                else
                                    yield
                                        InvocationArgumentTypeMismatch(
                                            functionName,
                                            argumentName,
                                            exprType,
                                            argumentType
                                        )
            }
        }


    let assignmentErrors (AssignmentNode(IdentifierNode(name), expression)) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx
            let runT = fun x -> ReaderT.run x ctx

            seq {
                match name |> SemanticContext.getSymbol ctx with
                | Ok(Variable variableType) ->
                    match runT (Resolve.expressionType expression) with
                    | Error errors -> yield! errors
                    | Ok expressionType ->
                        if variableType = expressionType then
                            ()
                        else
                            yield AssignmentWithWrongType(name, variableType, expressionType)
                | Ok(Function _) -> yield AssignmentToFunction(name)
                | Ok(Program) -> yield AssignmentToProgram
                | Ok(Argument _) -> yield AssignmentToArgument(name)
                | Error(error) -> yield error

                yield! run (expressionErrors expression)
            }
        }


    let rec conditionErrors (node: ConditionNode) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx

            let runT = fun x -> ReaderT.run x ctx

            seq {
                match node with
                | IfElseCondition(IfNode(ifExpr, ifBody), ElseNode(elseExpr, elseBody)) ->
                    yield! run (expressionErrors ifExpr)
                    yield! run (bodyErrors ifBody)
                    yield! run (expressionErrors elseExpr)
                    yield! run (bodyErrors elseBody)

                    let ifExprType = runT (Resolve.expressionType ifExpr) |> Validation.ofResult

                    let elseExprType = runT (Resolve.expressionType elseExpr) |> Validation.ofResult

                    match Validation.zip ifExprType elseExprType with
                    | Success(BooleanType, BooleanType) -> ()
                    | Failure resolveErrors -> yield! resolveErrors
                    | Success(ifType, BooleanType) -> yield InvalidConditionType ifType
                    | Success(BooleanType, elseType) -> yield InvalidConditionType elseType
                    | Success(ifType, elseType) ->
                        yield InvalidConditionType ifType
                        yield InvalidConditionType elseType
                | IfCondition(IfNode(ifExpr, ifBody)) ->
                    yield! run (expressionErrors ifExpr)
                    yield! run (bodyErrors ifBody)

                    match runT (Resolve.expressionType ifExpr) with
                    | Ok BooleanType -> ()
                    | Ok ifType -> yield InvalidConditionType ifType
                    | Error errors -> yield! errors
            }
        }

    and cycleErrors (CycleNode(body, expression)) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx

            let runT = fun x -> ReaderT.run x ctx

            seq {
                yield! run (bodyErrors body)
                yield! run (expressionErrors expression)

                match runT (Resolve.expressionType expression) with
                | Ok BooleanType -> ()
                | Ok ty -> yield InvalidConditionType ty
                | Error errors -> yield! errors
            }

        }


    and printErrors (PrintNode expressions) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx
            let runT = fun x -> ReaderT.run x ctx

            seq {
                for expression in expressions do
                    yield! run (expressionErrors expression)

                    match runT (Resolve.expressionType expression) with
                    | Error errors -> yield! errors
                    | _ -> ()
            }
        }

    and returnErrors (ReturnNode optExpression) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx
            let runT = fun x -> ReaderT.run x ctx

            seq {
                match ctx.ReturnType, optExpression with
                | Void, None -> ()
                | Void, Some _ -> yield ReturnedValueFromVoidFunction ctx.Name
                | ReturnType returnType, None -> yield ReturnedNothingFromValueReturningFunction(ctx.Name, returnType)
                | ReturnType returnType, Some(expression) ->
                    yield! run (expressionErrors expression)

                    match runT (Resolve.expressionType expression) with
                    | Error errors -> yield! errors
                    | Ok expressionType ->
                        if expressionType = returnType then
                            ()
                        else
                            yield ReturnTypeMismatch(ctx.Name, expressionType, returnType)
            }
        }

    and bodyErrors (BodyNode body) : Reader<_, _ seq> =
        body
        |> List.toSeq
        |> Seq.traverse (fun (statement: StatementNode) ->
            match statement with
            | AssignmentStatement node -> assignmentErrors node
            | ConditionStatement node -> conditionErrors node
            | CycleStatement node -> cycleErrors node
            | InvocationStatement node -> invocationArgumentErrors node
            | PrintStatement node -> printErrors node
            | ReturnStatement node -> returnErrors node)
        |>> Seq.concat
