[<AutoOpen>]
module LittleDuck.SemanticContext

open FSharpPlus
open FSharpPlus.Lens

open Ast
open SemanticAnalysisTypes

type SemanticContext =
    { Name: string
      ReturnType: FunctionReturnType
      SymbolTable: Map<string, Symbol> }

module SemanticContext =
    /// <summary>
    /// Initializes the SemanticContext with a given name and default values for
    /// `ReturnType` and `SymbolTable`. The `SymbolTable` is initialized with a mapping
    /// from the given name to the `Program` symbol.
    /// </summary>
    ///
    /// <param name="name">The name of the context currently being analyzed</param>
    ///
    /// <returns>A new SemanticContext</returns>
    let init name =
        { Name = name
          ReturnType = Void
          SymbolTable = Map([ name, Program ]) }

    // Lenses
    let inline _name f s =
        f s.Name <&> fun x -> { s with Name = x }

    let inline _returnType f s =
        f s.ReturnType <&> fun x -> { s with ReturnType = x }

    let inline _symbolTable f s =
        f s.SymbolTable <&> fun x -> { s with SymbolTable = x }

    [<AutoOpen>]
    module private Helpers =
        let addVariable varName varType ctx =
            ctx |> over _symbolTable (Map.add varName (Variable varType))

        let addFunction funName info ctx =
            ctx |> over _symbolTable (Map.add funName (Function info))

        let addArgument argName argType ctx =
            ctx |> over _symbolTable (Map.add argName (Argument argType))

        let tryGetSymbol state symbolName =
            state |> view _symbolTable |> Map.tryFind symbolName

    /// <summary>
    /// Defines a new variable within the given context,
    /// shadowing any previously defined symbols (except for the Program).
    /// </summary>
    /// <param name="varType">The type of the variable to be defined</param>
    /// <param name="varName">The name of the variable to be defined</param>
    /// <param name="ctx">The context within which to define the variable</param>
    ///
    /// <returns>
    /// Either an updated context with the newly defined variable or an error if the variable name
    /// conflicts with the Program.
    /// </returns>
    let defineVariable varType varName ctx =
        match varName |> tryGetSymbol ctx with
        | Some(Program) -> NameConflictsWithProgram varName |> Error
        | Some(Argument _)
        | Some(Variable _) // Allow shadowing of previous variables, arguments, and functions and functions
        | Some(Function _)
        | None -> ctx |> addVariable varName varType |> Ok

    /// <summary>
    /// Defines a new function within the given context, shadowing any previously defined symbols
    /// (except for the Program).
    /// This allows the introduction of a function in the current semantic context
    /// while ensuring it does not conflict with the existing program definition.
    /// </summary>
    ///
    /// <param name="declaration">The function declaration node
    /// containing details of the function to be defined</param>
    /// <param name="ctx">The context within which to define the function</param>
    ///
    /// <returns>
    /// Either an updated context with the newly defined function or an error if the function name
    /// conflicts with the Program.
    /// </returns>
    let defineFunction declaration ctx =
        let (FunctionDeclarationNode(_, IdentifierNode funName, _, _, _)) = declaration

        match funName |> tryGetSymbol ctx with
        | Some(Program) -> NameConflictsWithProgram funName |> Error
        | Some(Argument _)
        | Some(Variable _)
        | Some(Function _) // Allow shadowing of previous variables, arguments, and functions
        | None -> addFunction funName declaration ctx |> Ok

    /// <summary>
    /// Defines a new argument within a given context. The function allows the introduction of an argument
    /// in the current semantic context while ensuring it does not conflict with the existing program definition.
    /// </summary>
    ///
    /// <param name="node">The argument node containing the identifier and type of the argument to be defined</param>
    /// <param name="ctx">The context within which to define the argument</param>
    ///
    /// <returns>
    /// Either an updated context with the newly defined argument or an error if the argument name
    /// conflicts with the Program.
    /// </returns>
    let defineArgument (ArgumentNode(IdentifierNode name, argType) as node) ctx =
        match name |> tryGetSymbol ctx with
        | Some(Program) -> NameConflictsWithProgram name |> Error
        | Some(Argument _)
        | Some(Variable _) // Allow shadowing of previous variables, arguments, and functions
        | Some(Function _)
        | None -> ctx |> addArgument name argType |> Ok

    /// <summary>
    /// Tries to resolve a symbol by its name within the given `SemanticContext`, handling errors if resolution fails.
    /// </summary>
    ///
    /// <param name="ctx">Context from which to resolve the symbol</param>
    /// <param name="symbolName">Name of the symbol to resolve</param>
    ///
    /// <returns>
    /// Either the resolved symbol, or an error if it is not found.
    /// </returns>
    let getSymbol ctx symbolName =
        symbolName |> tryGetSymbol ctx |> Option.toResultWith (UnknownSymbol symbolName)
