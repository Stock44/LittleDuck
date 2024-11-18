[<AutoOpen>]
module LittleDuck.SemanticCube

let validateCondition ty =
    if ty = BooleanType then
        None
    else
        ty |> InvalidConditionType |> Some

let areTypesCompatible fromType toType =
    match fromType, toType with
    | IntType, IntType
    | FloatType, FloatType
    | FloatType, IntType
    | IntType, FloatType
    | StringType, StringType
    | IntType, StringType
    | FloatType, StringType
    | BooleanType, StringType -> true
    | _, _ -> false

let validateAddition lhs rhs =
    match lhs, rhs with
    | IntType, IntType -> Ok IntType
    | StringType, StringType -> Ok StringType
    | IntType, FloatType
    | FloatType, FloatType
    | FloatType, IntType -> Ok FloatType
    | lhs, rhs -> InvalidBinaryOperandTypes("+", rhs, lhs) |> Error


let validateSubtraction lhs rhs =
    match lhs, rhs with
    | IntType, IntType -> Ok IntType
    | IntType, FloatType
    | FloatType, IntType
    | FloatType, FloatType -> Ok FloatType
    | lhs, rhs -> InvalidBinaryOperandTypes("-", rhs, lhs) |> Error

let validateDivision lhs rhs =
    match lhs, rhs with
    | IntType, IntType
    | IntType, FloatType
    | FloatType, IntType
    | FloatType, FloatType -> Ok FloatType
    | lhs, rhs -> InvalidBinaryOperandTypes("/", rhs, lhs) |> Error

let validateMultiplication lhs rhs =
    match lhs, rhs with
    | IntType, IntType -> Ok IntType
    | IntType, FloatType
    | FloatType, IntType
    | FloatType, FloatType -> Ok FloatType
    | lhs, rhs -> InvalidBinaryOperandTypes("*", rhs, lhs) |> Error

let validateGreaterThan lhs rhs =
    match lhs, rhs with
    | IntType, IntType
    | IntType, FloatType
    | FloatType, IntType
    | FloatType, FloatType -> Ok BooleanType
    | lhs, rhs -> InvalidBinaryOperandTypes(">", rhs, lhs) |> Error

let validateLessThan lhs rhs =
    match lhs, rhs with
    | IntType, IntType
    | IntType, FloatType
    | FloatType, IntType
    | FloatType, FloatType -> Ok BooleanType
    | lhs, rhs -> InvalidBinaryOperandTypes("<", rhs, lhs) |> Error

let validateEquals lhs rhs =
    match lhs, rhs with
    | IntType, IntType
    | FloatType, FloatType
    | StringType, StringType
    | BooleanType, BooleanType
    | IntType, FloatType
    | FloatType, IntType -> Ok BooleanType
    | lhs, rhs -> InvalidBinaryOperandTypes("=", rhs, lhs) |> Error

let validateNotEqual lhs rhs =
    match lhs, rhs with
    | IntType, IntType
    | FloatType, FloatType
    | StringType, StringType
    | BooleanType, BooleanType
    | IntType, FloatType
    | FloatType, IntType -> Ok BooleanType
    | lhs, rhs -> InvalidBinaryOperandTypes("!=", rhs, lhs) |> Error
