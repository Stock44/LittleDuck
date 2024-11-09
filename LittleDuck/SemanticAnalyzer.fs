module LittleDuck.SemanticAnalyzer

open LittleDuck.Ast

open FSharpPlus


type Identifier =
    | Variable of TypeNode
    | Function of TypeNode * TypeNode list

and IdentifierTable = Map<string, Identifier>

module IdentifierTable =
    let unwrapIdentifier (IdentifierNode name) = name

    let unwrapVariableDeclaration (VariableDeclarationNode(varNames, varType)) =
        varNames |>> unwrapIdentifier |>> flip tuple2 varType

    let addVariableDeclarations (VariableDeclarationsNode declarations) x : Result<IdentifierTable, string> =
        declarations
        >>= unwrapVariableDeclaration
        |> List.fold
            (fun result (varName, varType) ->
                match result with
                | Error _ -> result
                | Ok table ->
                    if table |> Map.containsKey varName then
                        Error "Variable {varName} has already been declared."
                    else
                        table |> Map.add varName (Variable varType) |> Ok)
            (Ok x)
