module LittleDuck.Ast

type IdentifierNode = IdentifierNode of string

type ValueNode =
    | Float of float
    | Int of int
    | String of string
    | Boolean of bool

type CTENode =
    | NegativeCTE of ValueNode
    | PositiveCTE of ValueNode
    | ValueCTE of ValueNode
    | IdentifierCTE of IdentifierNode

// Recursive on ExpressionNode

type FactorNode =
    | ParenthesizedExprFactor of ExpressionNode
    | CTEFactor of CTENode

and TermNode =
    | FactorTerm of FactorNode
    | DivisionTerm of TermNode * FactorNode
    | MultiplicationTerm of TermNode * FactorNode

and ExpNode =
    | TermExp of TermNode
    | SumExp of ExpNode * TermNode
    | SubtractionExp of ExpNode * TermNode

and ExpressionNode =
    | LessThanExpression of ExpNode * ExpNode
    | MoreThanExpression of ExpNode * ExpNode
    | NotEqualExpression of ExpNode * ExpNode
    | ExpExpression of ExpNode

// End of recursion

type PrintNode = PrintNode of ExpressionNode list

type InvocationNode = InvocationNode of IdentifierNode * ExpressionNode list

// Recursive on BodyNode

type CycleNode = CycleNode of BodyNode * ExpressionNode

and IfNode = IfNode of ExpressionNode * BodyNode

and ElseNode = ElseNode of ExpressionNode * BodyNode

and ConditionNode =
    | IfElseCondition of IfNode * ElseNode
    | IfCondition of IfNode

and AssignmentNode = AssignmentNode of IdentifierNode * ExpressionNode

and StatementNode =
    | AssignmentStatement of AssignmentNode
    | ConditionStatement of ConditionNode
    | CycleStatement of CycleNode
    | InvocationStatement of InvocationNode
    | PrintStatement of PrintNode
    | ReturnStatement of ExpressionNode option

and BodyNode = BodyNode of StatementNode list

// End of recursion

type TypeNode =
    | FloatType
    | IntType
    | StringType
    | BooleanType

type VariableDeclarationNode = VariableDeclarationNode of IdentifierNode list * TypeNode

type VariableDeclarationsNode = VariableDeclarationsNode of VariableDeclarationNode list

type ArgumentNode = ArgumentNode of IdentifierNode * TypeNode

type FunctionReturnType =
    | Void
    | ReturnType of TypeNode

type FunctionDeclarationNode =
    | FunctionDeclarationNode of FunctionReturnType *  IdentifierNode * ArgumentNode list * VariableDeclarationsNode * BodyNode

type FunctionDeclarationsNode = FunctionDeclarationsNode of FunctionDeclarationNode list

type ProgramNode = ProgramNode of IdentifierNode * VariableDeclarationsNode * FunctionDeclarationsNode * BodyNode
