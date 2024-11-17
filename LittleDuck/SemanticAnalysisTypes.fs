[<AutoOpen>]
module LittleDuck.SemanticAnalysisTypes

open LittleDuck.Ast

/// <summary>
/// Represents various kinds of semantic errors that can occur during the semantic analysis phase.
/// </summary>
type SemanticError =
    | UnknownSymbol of symbolName: string
    | NameConflictsWithProgram of symbolName: string
    | ProgramUsedAsValue
    | FunctionUsedAsValue of functionName: string
    | VoidFunctionUsedAsValue of functionName: string
    | InvalidBinaryOperandTypes of operator: string * lhsType: TypeNode * rhsType: TypeNode
    | AssignmentWithWrongType of variableName: string * variableType: TypeNode * receivedType: TypeNode
    | AssignmentToFunction of functionName: string
    | AssignmentToProgram
    | AssignmentToArgument of argumentName: string
    | InvalidConditionType of receivedType: TypeNode
    | InvokedVariable of variableName: string
    | InvokedArgument of argumentName: string
    | InvokedProgram
    | InvocationArgumentCountMismatch of functionName: string * receivedArgumentCount: int * expectedArgumentCount: int
    | InvocationArgumentTypeMismatch of
        functionName: string *
        argumentName: string *
        receivedArgumentType: TypeNode *
        expectedArgumentType: TypeNode
    | ReturnedValueFromVoidFunction of functionName: string
    | ReturnedNothingFromValueReturningFunction of functionName: string * expectedType: TypeNode
    | ReturnTypeMismatch of functionName: string * returnedType: TypeNode * expectedType: TypeNode

type Symbol =
    | Program
    | Variable of TypeNode
    | Argument of TypeNode
    | Function of FunctionDeclarationNode
