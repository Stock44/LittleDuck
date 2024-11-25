module LittleDuck.Encoder
open FSharpPlus

type Operator =
    | AddArg
    | Call
    | JumpIf
    | Negate
    | Sum
    | Multiplication
    | Division
    | GreaterThan
    | LessThan
    | Equals
    | NotEquals

type Return =
    | Value of obj
    | Address of int

type Operation = Operation of op: Operator * v1: obj * v2: obj * ret: Return

module Encode =
    let cte (node: CTENode) =
        match node with
        | NegativeCTE valueNode -> valueNode
        | PositiveCTE valueNode -> failwith "todo"
        | ValueCTE valueNode -> failwith "todo"
        | IdentifierCTE identifierNode -> failwith "todo"
        | InvocationCTE invocationNode -> failwith "todo"

    let factor (node: FactorNode) =
        match node with
        | ParenthesizedExprFactor expressionNode -> failwith "todo"
        | CTEFactor cteNode -> failwith "todo"

    let term (node: TermNode) =
        match node with
        | FactorTerm factorNode -> failwith "todo"
        | DivisionTerm(termNode, factorNode) -> failwith "todo"
        | MultiplicationTerm(termNode, factorNode) -> failwith "todo"

    let exp (node: ExpNode) =
        match node with
        | TermExp termNode -> failwith "todo"
        | SumExp(expNode, termNode) -> failwith "todo"
        | SubtractionExp(expNode, termNode) -> failwith "todo"

    let expression (node: ExpressionNode) =
        match node with
        | LessThanExpression(expNode, node) -> failwith "todo"
        | GreaterThanExpression(expNode, node) -> failwith "todo"
        | NotEqualExpression(expNode, node) -> failwith "todo"
        | EqualsExpression(expNode, node) -> failwith "todo"
        | ExpExpression expNode -> failwith "todo"

    let body (BodyNode statements) =
        monad.plus {
            let! statement = statements

            match statement with
            | AssignmentStatement (AssignmentNode (IdentifierNode name, expression)) ->  failwith "todo"
            | ConditionStatement conditionNode -> failwith "todo"
            | CycleStatement cycleNode -> failwith "todo"
            | InvocationStatement invocationNode -> failwith "todo"
            | PrintStatement printNode -> failwith "todo"
            | ReturnStatement returnNode -> failwith "todo"
        }
