[<AutoOpen>]
module LittleDuck.ErrorCollectors

open FSharpPlus
open FSharpPlus.Data


[<RequireQualifiedAccess>]
module Collect =
    let assignmentErrors (AssignmentNode(IdentifierNode(name), expression)) =
        monad' {
            let! ctx = Reader.ask

            seq {
                match name |> SemanticContext.getSymbol ctx with
                | Ok(Variable variableType) ->
                    match ReaderT.run (Resolve.expressionType expression) ctx with
                    | Error error -> yield error
                    | Ok expressionType ->
                        if variableType = expressionType then
                            ()
                        else
                            yield AssignmentWithWrongType(name, variableType, expressionType)
                | Ok(Function _) -> yield AssignmentToFunction(name)
                | Ok(Program) -> yield AssignmentToProgram
                | Ok(Argument _) -> yield AssignmentToArgument(name)
                | Error(error) -> yield error
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
                    yield! run (bodyErrors ifBody)
                    yield! run (bodyErrors elseBody)

                    let ifExprType = runT (Resolve.expressionType ifExpr) |> Validation.validationNel

                    let elseExprType = runT (Resolve.expressionType elseExpr) |> Validation.validationNel

                    match Validation.zip ifExprType elseExprType with
                    | Success(BooleanType, BooleanType) -> ()
                    | Failure resolveErrors -> yield! (resolveErrors |> NonEmptyList.toList)
                    | Success(ifType, BooleanType) -> yield InvalidConditionType ifType
                    | Success(BooleanType, elseType) -> yield InvalidConditionType elseType
                    | Success(ifType, elseType) ->
                        yield InvalidConditionType ifType
                        yield InvalidConditionType elseType
                | IfCondition(IfNode(ifExpr, ifBody)) ->
                    yield! run (bodyErrors ifBody)

                    let exprType = runT (Resolve.expressionType ifExpr)

                    match exprType with
                    | Ok BooleanType -> ()
                    | Ok ifType -> yield InvalidConditionType ifType
                    | Error error -> yield error
            }
        }

    and cycleErrors (CycleNode(body, expression)) =
        monad' {
            let! ctx = Reader.ask

            let run = fun x -> Reader.run x ctx

            let runT = fun x -> ReaderT.run x ctx

            seq {
                yield! run (bodyErrors body)

                let exprType = runT (Resolve.expressionType expression)

                match exprType with
                | Ok BooleanType -> ()
                | Ok ty -> yield InvalidConditionType ty
                | Error error -> yield error
            }

        }

    and invocationErrors (InvocationNode(IdentifierNode functionName, arguments)) =
        monad' {
            let! ctx = Reader.ask

            let runT = fun x -> ReaderT.run x ctx

            let symbol = functionName |> SemanticContext.getSymbol ctx

            seq {
                match symbol with
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
                            match runT (Resolve.expressionType expression) with
                            | Error error -> yield error
                            | Ok exprType ->
                                if exprType = argumentType then
                                    ()
                                else
                                    yield InvocationArgumentTypeMismatch(functionName, argumentName, exprType, argumentType)
            }
        }


    and printErrors (PrintNode expressions) =
        monad' {
            let! ctx = Reader.ask

            let runT = fun x -> ReaderT.run x ctx

            seq {
                for expression in expressions do
                    match runT (Resolve.expressionType expression) with
                    | Error error -> yield error
                    | _ -> ()
            }
        }

    and returnErrors (ReturnNode optExpression) =
        monad' {
            let! ctx = Reader.ask

            let runT = fun x -> ReaderT.run x ctx

            seq {
                match ctx.ReturnType, optExpression with
                | Void, None -> ()
                | Void, Some _ -> yield ReturnedValueFromVoidFunction ctx.Name
                | ReturnType returnType, None -> yield ReturnedNothingFromValueReturningFunction(ctx.Name, returnType)
                | ReturnType returnType, Some(expression) ->
                    match runT (Resolve.expressionType expression) with
                    | Error error -> yield error
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
            | InvocationStatement node -> invocationErrors node
            | PrintStatement node -> printErrors node
            | ReturnStatement node -> returnErrors node)
        |>> Seq.concat

