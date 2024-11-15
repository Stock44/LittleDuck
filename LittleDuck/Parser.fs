[<AutoOpen>]
module LittleDuck.Parser

open System
open Ast

open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

[<AutoOpen>]
module private Helpers =
    let operator representation pBase baseAstNode =
        ((skipString representation >>. spaces >>. pBase)
         |>> (fun rhs -> (fun lhs -> (lhs, rhs) |> baseAstNode)))

    let leftAssociativeOp (pOp: Parser<_ -> _, _>) pBase =
        let pTail, pTailRef = createParserForwardedToRef ()

        pTailRef.Value <-
            spaces >>? tuple2 pOp (pTail <|> preturn id)
            |>> (fun (inner, outer) -> inner >> outer)

        tuple2 pBase (pTail <|> preturn id) |>> fun (lhs, next) -> lhs |> next

module Identifier =
    let parse: Parser<_> =
        regex "[^\d\\+\-/*><=\s\".!:,()[\];{}][^\\+\-/*><=\s\".!:,()[\];{}]*"
        |>> IdentifierNode
        <?> "identifier"

module Value =
    let pNumber =
        numberLiteral NumberLiteralOptions.AllowFraction "value"
        |>> fun nl ->
            if nl.IsInteger then
                Int(Int32.Parse(nl.String))
            else
                Float(Double.Parse(nl.String))

    let pString = skipChar '\"' >>. manyCharsTill anyChar (skipChar '"') |>> String

    let pBoolean =
        (skipString "true" >>% (Boolean true))
        <|> (skipString "false" >>% (Boolean false))

    let parse = pNumber <|> pString <|> pBoolean

module CTE =
    let pNegativeCTE = (skipString "-" >>. spaces >>. Value.parse) |>> NegativeCTE

    let pPositiveCTE = (skipString "+" >>. spaces >>. Value.parse) |>> PositiveCTE

    let pValueCTE = Value.parse |>> ValueCTE

    let pIdentifierCTE = Identifier.parse |>> IdentifierCTE

    let parse = pNegativeCTE <|> pPositiveCTE <|> pValueCTE <|> pIdentifierCTE


let pExpression, pExpressionRef = createParserForwardedToRef<ExpressionNode, _> ()

module Factor =
    let pParenthesizedExpressionFactor =
        between (skipString "(" >>. spaces) (spaces .>> skipString ")") pExpression
        |>> ParenthesizedExprFactor

    let pCTEFactor = CTE.parse |>> CTEFactor

    let parse = pParenthesizedExpressionFactor <|> pCTEFactor

module Term =
    let pDivOp = operator "/" Factor.parse DivisionTerm
    let pMulOp = operator "*" Factor.parse MultiplicationTerm

    let pOp = pDivOp <|> pMulOp

    let parse = leftAssociativeOp pOp (Factor.parse |>> FactorTerm)

module Exp =
    let pSumOp = operator "+" Term.parse SumExp
    let pSubtractionOp = operator "-" Term.parse SubtractionExp

    let pOp = pSumOp <|> pSubtractionOp

    let parse = leftAssociativeOp pOp (Term.parse |>> TermExp)

module Expression =
    let pLessThanExpression =
        tuple2 (Exp.parse .>>? spaces .>>? skipString "<" .>> spaces) Exp.parse
        |>> LessThanExpression

    let pMoreThanExpression =
        tuple2 (Exp.parse .>>? spaces .>>? skipString ">" .>> spaces) Exp.parse
        |>> MoreThanExpression

    let pNotEqualExpression =
        tuple2 (Exp.parse .>>? spaces .>>? skipString "!=" .>> spaces) Exp.parse
        |>> NotEqualExpression

    let pExpExpression = Exp.parse |>> ExpExpression

    pExpressionRef.Value <-
        pLessThanExpression
        <|> pMoreThanExpression
        <|> pNotEqualExpression
        <|> pExpExpression

    let parse = pExpression

module Print =
    let parse =
        skipString "print"
        >>. spaces
        >>. skipChar '('
        >>. spaces
        >>. sepBy Expression.parse (spaces >>? skipChar ',' >>. spaces)
        .>> spaces
        .>> skipChar ')'
        |>> PrintNode

module Invocation =
    let parse =
        tuple2
            (Identifier.parse .>>? spaces .>>? skipChar '(' .>> spaces)
            (sepBy Expression.parse (spaces >>? skipChar ',' >>. spaces))
        .>> spaces
        .>> skipChar ')'
        |>> InvocationNode

let pBody, pBodyRef = createParserForwardedToRef<BodyNode, _> ()

module Cycle =
    let parse =
        tuple2
            (skipString "while" >>. spaces >>. pBody
             .>> spaces
             .>> skipString "do"
             .>> spaces
             .>> skipChar '('
             .>> spaces)
            (Expression.parse .>> spaces .>> skipChar ')')
        |>> CycleNode

module If =
    let parse =
        tuple2
            (skipString "if" >>. spaces >>. skipChar '(' >>. spaces >>. Expression.parse
             .>> spaces
             .>> skipChar ')'
             .>> spaces)
            pBody
        |>> IfNode

module Else =
    let parse =
        tuple2
            (skipString "else" >>. spaces >>. skipChar '(' >>. spaces >>. Expression.parse
             .>> spaces
             .>> skipChar ')'
             .>> spaces)
            pBody
        |>> ElseNode

module Condition =
    let pIfElseCondition = tuple2 (If.parse .>>? spaces) Else.parse |>> IfElseCondition

    let pIfCondition = If.parse |>> IfCondition

    let parse =
        pipe2 If.parse (opt (spaces >>? Else.parse)) (fun ifNode elseNode ->
            match elseNode with
            | None -> ifNode |> IfCondition
            | Some elseNode -> IfElseCondition(ifNode, elseNode))

module Assignment =
    let parse =
        tuple2 (Identifier.parse .>> spaces .>> skipChar '=' .>> spaces) Expression.parse
        |>> AssignmentNode

module Return =
    let parse =
        skipString "return"
        >>. ((spaces1 >>? Expression.parse |>> (Some >> ReturnNode))
             <|> preturn (ReturnNode None))

module Statement =
    let pAssignmentStatement = Assignment.parse |>> AssignmentStatement

    let pConditionStatement = Condition.parse |>> ConditionStatement

    let pCicleStatement = Cycle.parse |>> CycleStatement

    let pInvocationStatement = Invocation.parse |>> InvocationStatement

    let pPrintStatement = Print.parse |>> PrintStatement

    let pReturnStatement = Return.parse |>> ReturnStatement

    let parse =
        (pPrintStatement
         <|> pReturnStatement
         <|> pCicleStatement
         <|> pConditionStatement
         <|> pInvocationStatement
         <|> pAssignmentStatement)
        <?> "statement"

module Body =
    pBodyRef.Value <-
        skipChar '{'
        >>. spaces
        >>. many (Statement.parse .>> spaces .>> skipChar ';' .>> spaces)
        .>> skipChar '}'
        |>> BodyNode

    let parse = pBody

module Type =
    let pIntType = skipString "int" >>% IntType

    let pFloatType = skipString "float" >>% FloatType

    let pBooleanType = skipString "bool" >>% BooleanType

    let pStringType = skipString "string" >>% StringType

    let parse = pIntType <|> pFloatType <|> pBooleanType <|> pStringType

module VariableDeclaration =
    let parse =
        tuple2
            (sepBy (Identifier.parse .>> spaces) (skipChar ',' >>. spaces) .>>? skipChar ':'
             .>> spaces)
            Type.parse
        |>> VariableDeclarationNode

module VariableDeclarations =
    let parse =
        many (VariableDeclaration.parse .>> spaces .>> skipChar ';' .>> spaces)
        |>> VariableDeclarationsNode

module Argument =
    let parse =
        tuple2 (Identifier.parse .>> spaces .>> skipChar ':' .>> spaces) Type.parse
        |>> ArgumentNode

module FunctionDeclaration =
    let pArgumentList = (sepBy Argument.parse (spaces .>> skipChar ',' .>> spaces))

    let pVoid = skipString "void" >>% Void

    let pReturnType = pVoid <|> (Type.parse |>> ReturnType)

    let parse =
        tuple5
            (pReturnType .>>? spaces1)
            (Identifier.parse .>> spaces .>> skipChar '(' .>> spaces)
            (pArgumentList .>> spaces .>> skipChar ')' .>> spaces .>> skipChar '[' .>> spaces)
            (VariableDeclarations.parse .>> spaces)
            (Body.parse .>> spaces .>> skipChar ']')
        |>> FunctionDeclarationNode

module FunctionDeclarations =
    let parse =
        many (FunctionDeclaration.parse .>> spaces .>> skipChar ';' .>> spaces)
        |>> FunctionDeclarationsNode

module Program =
    let parse =
        tuple4
            (skipString "program" >>. spaces1 >>. Identifier.parse
             .>> spaces
             .>> skipChar ';'
             .>> spaces)
            (VariableDeclarations.parse .>> spaces)
            (FunctionDeclarations.parse .>> spaces .>> skipString "main" .>> spaces)
            (Body.parse .>> spaces .>> skipString "end" .>> spaces)
        |>> ProgramNode
