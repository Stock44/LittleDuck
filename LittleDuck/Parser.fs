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
        ((skipString representation >>? spaces >>? pBase)
         |>> (fun rhs -> (fun lhs -> (lhs, rhs) |> baseAstNode)))

    let leftAssociativeOp (pOp: Parser<_ -> _, _>) pBase =
        let pTail, pTailRef = createParserForwardedToRef ()

        pTailRef.Value <-
            spaces >>? tuple2 pOp (pTail <|> preturn id)
            |>> (fun (inner, outer) -> inner >> outer)

        tuple2 pBase (pTail <|> preturn id) |>> fun (lhs, next) -> lhs |> next

module Identifier =
    let parse =
        regex "[^\d\\+\-/*><=\s\".!:,()[\];{}][^\\+\-/*><=\s\".!:,()[\];{}]*"
        |>> IdentifierNode
        <?> "identifier"

let pComment = skipString "//" >>. manyCharsTill anyChar newline |>> ignore

let pMultilineComment =
    skipString "/*" >>. manyCharsTill anyChar (skipString "*/") |>> ignore

let pWhitespace =
    (spaces >>? (pComment <|> pMultilineComment) >>. spaces) <|> spaces

let pWhitespace1 =
    (spaces1 >>? (pComment <|> pMultilineComment) >>. spaces) <|> spaces

module Value =
    let pNumber =
        numberLiteral NumberLiteralOptions.AllowFraction "number"
        |>> fun nl ->
            if nl.IsInteger then
                Int(Int32.Parse(nl.String))
            else
                Float(Double.Parse(nl.String))

    let pString = skipChar '"' >>. manyCharsTill anyChar (skipChar '"') |>> String

    let pBoolean =
        (skipString "true" >>% (Boolean true))
        <|> (skipString "false" >>% (Boolean false))

    let parse = pNumber <|> pString <|> pBoolean

let pExpression, pExpressionRef = createParserForwardedToRef<ExpressionNode, _> ()

module Invocation =
    let parse =
        tuple2
            (Identifier.parse .>>? pWhitespace .>>? skipChar '(' .>> pWhitespace)
            (sepBy pExpression (pWhitespace >>? skipChar ',' >>. pWhitespace))
        .>> pWhitespace
        .>> skipChar ')'
        |>> InvocationNode

module CTE =
    let pNegativeCTE = (skipString "-" >>. pWhitespace >>. Value.parse) |>> NegativeCTE

    let pPositiveCTE = (skipString "+" >>. pWhitespace >>. Value.parse) |>> PositiveCTE

    let pValueCTE = Value.parse |>> ValueCTE

    let pInvocationCTE = Invocation.parse |>> InvocationCTE

    let pIdentifierCTE = Identifier.parse |>> IdentifierCTE

    let parse =
        pNegativeCTE
        <|> pPositiveCTE
        <|> pValueCTE
        <|> pInvocationCTE
        <|> pIdentifierCTE



module Factor =
    let pParenthesizedExpressionFactor =
        between (skipString "(" >>. pWhitespace) (pWhitespace .>> skipString ")") pExpression
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
        tuple2 (Exp.parse .>>? pWhitespace .>>? skipString "<" .>> pWhitespace) Exp.parse
        |>> LessThanExpression

    let pMoreThanExpression =
        tuple2 (Exp.parse .>>? pWhitespace .>>? skipString ">" .>> pWhitespace) Exp.parse
        |>> GreaterThanExpression

    let pNotEqualExpression =
        tuple2 (Exp.parse .>>? pWhitespace .>>? skipString "!=" .>> pWhitespace) Exp.parse
        |>> NotEqualExpression

    let pEqualExpression =
        tuple2 (Exp.parse .>>? pWhitespace .>>? skipString "==" .>> pWhitespace) Exp.parse
        |>> EqualsExpression

    let pExpExpression = Exp.parse |>> ExpExpression

    pExpressionRef.Value <-
        pLessThanExpression
        <|> pMoreThanExpression
        <|> pNotEqualExpression
        <|> pEqualExpression
        <|> pExpExpression

    let parse = pExpression

module Print =
    let parse =
        skipString "print"
        >>. pWhitespace
        >>. skipChar '('
        >>. pWhitespace
        >>. sepBy Expression.parse (pWhitespace >>? skipChar ',' >>. pWhitespace)
        .>> pWhitespace
        .>> skipChar ')'
        |>> PrintNode


let pBody, pBodyRef = createParserForwardedToRef<BodyNode, _> ()

module Cycle =
    let parse =
        tuple2
            (skipString "while" >>. pWhitespace >>. pBody
             .>> pWhitespace
             .>> skipString "do"
             .>> pWhitespace
             .>> skipChar '('
             .>> pWhitespace)
            (Expression.parse .>> pWhitespace .>> skipChar ')')
        |>> CycleNode

module If =
    let parse =
        tuple2
            (skipString "if"
             >>. pWhitespace
             >>. skipChar '('
             >>. pWhitespace
             >>. Expression.parse
             .>> pWhitespace
             .>> skipChar ')'
             .>> pWhitespace)
            pBody
        |>> IfNode

module Else =
    let parse =
        tuple2
            (skipString "else"
             >>. pWhitespace
             >>. skipChar '('
             >>. pWhitespace
             >>. Expression.parse
             .>> pWhitespace
             .>> skipChar ')'
             .>> pWhitespace)
            pBody
        |>> ElseNode

module Condition =
    let pIfElseCondition =
        tuple2 (If.parse .>>? pWhitespace) Else.parse |>> IfElseCondition

    let pIfCondition = If.parse |>> IfCondition

    let parse =
        pipe2 If.parse (opt (pWhitespace >>? Else.parse)) (fun ifNode elseNode ->
            match elseNode with
            | None -> ifNode |> IfCondition
            | Some elseNode -> IfElseCondition(ifNode, elseNode))

module Assignment =
    let parse =
        tuple2 (Identifier.parse .>> pWhitespace .>> skipChar '=' .>> pWhitespace) Expression.parse
        |>> AssignmentNode

module Return =
    let parse =
        skipString "return"
        >>. ((pWhitespace1 >>? Expression.parse |>> (Some >> ReturnNode))
             <|> preturn (ReturnNode None))

module Statement =
    let pAssignmentStatement =
        Assignment.parse .>> pWhitespace .>> skipChar ';'
        |>> (AssignmentStatement >> Some)

    let pConditionStatement = Condition.parse |>> (ConditionStatement >> Some)

    let pCicleStatement = Cycle.parse |>> (CycleStatement >> Some)

    let pInvocationStatement =
        Invocation.parse .>> pWhitespace .>> skipChar ';'
        |>> (InvocationStatement >> Some)

    let pPrintStatement =
        Print.parse .>> pWhitespace .>> skipChar ';' |>> (PrintStatement >> Some)

    let pReturnStatement =
        Return.parse .>> pWhitespace .>> skipChar ';' |>> (ReturnStatement >> Some)

    let pEmptyStatement = pWhitespace >>. skipChar ';' >>% None

    let parse =
        (pPrintStatement
         <|> pReturnStatement
         <|> pCicleStatement
         <|> pConditionStatement
         <|> pInvocationStatement
         <|> pAssignmentStatement
         <|> pEmptyStatement)
        <?> "statement"

module Body =
    pBodyRef.Value <-
        skipChar '{' >>. pWhitespace >>. many (Statement.parse .>> pWhitespace)
        .>> skipChar '}'
        |>> (List.choose id >> BodyNode)

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
            (sepBy (Identifier.parse .>> pWhitespace) (skipChar ',' >>. pWhitespace)
             .>>? skipChar ':'
             .>> pWhitespace)
            Type.parse
        |>> VariableDeclarationNode

module VariableDeclarations =
    let parse =
        many (VariableDeclaration.parse .>> pWhitespace .>> skipChar ';' .>> pWhitespace)
        |>> VariableDeclarationsNode

module Argument =
    let parse =
        tuple2 (Identifier.parse .>> pWhitespace .>> skipChar ':' .>> pWhitespace) Type.parse
        |>> ArgumentNode

module FunctionDeclaration =
    let pArgumentList =
        (sepBy Argument.parse (pWhitespace .>> skipChar ',' .>> pWhitespace))

    let pVoid = skipString "void" >>% Void

    let pReturnType = pVoid <|> (Type.parse |>> ReturnType)

    let parse =
        tuple5
            (pReturnType .>>? pWhitespace1)
            (Identifier.parse .>> pWhitespace .>> skipChar '(' .>> pWhitespace)
            (pArgumentList
             .>> pWhitespace
             .>> skipChar ')'
             .>> pWhitespace
             .>> skipChar '['
             .>> pWhitespace)
            (VariableDeclarations.parse .>> pWhitespace)
            (Body.parse .>> pWhitespace .>> skipChar ']')
        |>> FunctionDeclarationNode

module FunctionDeclarations =
    let parse =
        many (FunctionDeclaration.parse .>> pWhitespace .>> skipChar ';' .>> pWhitespace)
        |>> FunctionDeclarationsNode

module Program =
    let parse =
        tuple4
            (skipString "program" >>. pWhitespace1 >>. Identifier.parse
             .>> pWhitespace
             .>> skipChar ';'
             .>> pWhitespace)
            (VariableDeclarations.parse .>> pWhitespace)
            (FunctionDeclarations.parse .>> pWhitespace .>> skipString "main" .>> pWhitespace)
            (Body.parse .>> pWhitespace .>> skipString "end" .>> pWhitespace)
        |>> ProgramNode

module LittleDuck =
    let parse program =
        match run (Program.parse .>> eof) program with
        | Success(ast, _, _) -> ast |> Result.Ok
        | Failure(error, _, _) -> error |> Result.Error
