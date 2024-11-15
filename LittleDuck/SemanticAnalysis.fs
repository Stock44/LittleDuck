[<AutoOpen>]
module LittleDuck.SemanticAnalysis

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens

type SemanticAnalysis =
    { Context: SemanticContext
      Errors: SemanticError list }

[<RequireQualifiedAccess>]
module SemanticAnalysis =
    // Initializer
    let init programName =
        { Context = SemanticContext.init programName
          Errors = [] }

    // Lenses
    let inline _context f a =
        f a.Context <&> fun x -> { a with Context = x }

    let inline _errors f a =
        f a.Errors <&> fun x -> { a with Errors = x }

    let inline setlContext l = setl (_context << l)

    [<AutoOpen>]
    module private Helpers =
        let unwrapIdentifier (IdentifierNode name) = name

        let reportError error state =
            state |> over _errors (fun x -> error :: x)

        let foldResult f state x =
            match f state.Context x with
            | Error error -> reportError error state
            | Ok nextCtx -> state |> setl _context nextCtx

        let foldResults f state xs = xs |> Seq.fold (foldResult f) state

    // Processors
    let processVariableDeclaration state (VariableDeclarationNode(identifiers, ty)) =
        identifiers |>> unwrapIdentifier
        |> foldResults (fun ctx x -> SemanticContext.defineVariable ty x ctx) state

    let processVariableDeclarations (VariableDeclarationsNode declarations) state =
        declarations |> List.fold processVariableDeclaration state

    let processArguments arguments state =
        arguments
        |> foldResults (fun ctx x -> SemanticContext.defineArgument x ctx) state

    let processFunctionDeclaration functionDeclaration state =
        functionDeclaration |> foldResult (flip SemanticContext.defineFunction) state

    let processBody body state =
        Reader.run (Collect.bodyErrors body) state.Context
        |> fold (flip reportError) state

    let scopeTo (FunctionDeclarationNode(returnType, IdentifierNode name, arguments, variableDeclarations, _)) state =
        state
        |> setlContext SemanticContext._name name
        |> setlContext SemanticContext._returnType returnType
        |> setl _errors []
        |> processArguments arguments
        |> processVariableDeclarations variableDeclarations

    let subsume other state =
        state |> over _errors (fun x -> (view _errors other) @ x)

    let processFunctionDeclarations declarations state =
        declarations
        |> fold
            (fun state declaration ->
                let (FunctionDeclarationNode(_, _, _, _, body)) = declaration

                let state = state |> processFunctionDeclaration declaration

                let scopedState = state |> scopeTo declaration |> processBody body

                state |> subsume scopedState)
            state

    let processProgram
        (ProgramNode(IdentifierNode(programName),
                     variableDeclarations,
                     FunctionDeclarationsNode(functionDeclarations),
                     body))
        =
        init programName
        |> processVariableDeclarations variableDeclarations
        |> processFunctionDeclarations functionDeclarations
        |> processBody body
