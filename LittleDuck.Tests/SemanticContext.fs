module LittleDuck.Tests.SemanticContext

open FSharpPlus
open FSharpPlus.Lens

open LittleDuck.Ast
open LittleDuck.SemanticAnalysisTypes
open LittleDuck.SemanticContext

open Xunit
open Swensen.Unquote

[<Fact>]
let ``init creates an empty semantic context`` () =
    let ctx = SemanticContext.init "LoremIpsum"

    test <@ ctx.Name = "LoremIpsum" @>
    test <@ ctx.ReturnType = Void @>
    test <@ ctx.SymbolTable = Map([ "LoremIpsum", Program ]) @>

[<Fact>]
let ``view returns correct name`` () =
    let ctx = SemanticContext.init "TestName"
    let retrieved = view SemanticContext._name ctx
    test <@ retrieved = "TestName" @>

[<Fact>]
let ``view returns correct returnType`` () =
    let ctx = SemanticContext.init "TestName"
    let retrieved = view SemanticContext._returnType ctx
    test <@ retrieved = Void @>

[<Fact>]
let ``view returns correct symbolTable`` () =
    let ctx = SemanticContext.init "TestName"
    let retrieved = view SemanticContext._symbolTable ctx
    test <@ retrieved = Map([ "TestName", Program ]) @>

[<Fact>]
let ``set updates name correctly`` () =
    let ctx = SemanticContext.init "OriginalName"
    let updatedCtx = setl SemanticContext._name "NewName" ctx
    let retrieved = view SemanticContext._name updatedCtx
    test <@ retrieved = "NewName" @>

[<Fact>]
let ``set updates returnType correctly`` () =
    let ctx = SemanticContext.init "TestName"
    let updatedCtx = setl SemanticContext._returnType (ReturnType IntType) ctx
    let retrieved = view SemanticContext._returnType updatedCtx
    test <@ retrieved = ReturnType IntType @>

[<Fact>]
let ``set updates symbolTable correctly`` () =
    let ctx = SemanticContext.init "TestName"

    let updatedCtx =
        setl SemanticContext._symbolTable (Map([ "key", Variable(IntType) ])) ctx

    let retrieved = view SemanticContext._symbolTable updatedCtx

    test <@ retrieved = Map([ "key", Variable(IntType) ]) @>

let variableDefinitions: obj[] list =
    [ [| "test"; IntType |]
      [| "test2"; FloatType |]
      [| "test3"; BooleanType |]
      [| "asdf"; StringType |]
      [| "wowowo"; FloatType |]
      [| "hwafa"; BooleanType |]
      [| "HEHE"; IntType |] ]

[<Theory>]
[<MemberData(nameof variableDefinitions)>]
let ``defineVariable always succeeds on empty context with program name other than the variable name`` name varType =
    let ctx = SemanticContext.init (name + "_program")

    test <@ ctx |> SemanticContext.defineVariable varType name |> Result.isOk @>


[<Theory>]
[<MemberData(nameof variableDefinitions)>]
let ``defineVariable always succeeds on empty context with different program name even when shadowing other variables``
    name
    varType
    =
    let ctx = SemanticContext.init (name + "_program")

    test
        <@
            ctx
            |> SemanticContext.defineVariable varType name
            |> Result.bind (SemanticContext.defineVariable varType name)
            |> Result.isOk
        @>

[<Theory>]
[<MemberData(nameof variableDefinitions)>]
let ``defineVariable always fails on empty context that has the same program name as the variable name`` name varType =
    let ctx = SemanticContext.init name

    test <@ ctx |> SemanticContext.defineVariable varType name |> Result.isError @>

let functionDefinitions: obj[] list =
    [ [| FunctionDeclarationNode(Void, IdentifierNode "asdfasdf", [], VariableDeclarationsNode [], BodyNode []) |]
      [| FunctionDeclarationNode(Void, IdentifierNode "amogus", [], VariableDeclarationsNode [], BodyNode []) |]
      [| FunctionDeclarationNode(Void, IdentifierNode "hawktuah", [], VariableDeclarationsNode [], BodyNode []) |]
      [| FunctionDeclarationNode(Void, IdentifierNode "loremlorem", [], VariableDeclarationsNode [], BodyNode []) |]
      [| FunctionDeclarationNode(Void, IdentifierNode "kd92", [], VariableDeclarationsNode [], BodyNode []) |]
      [| FunctionDeclarationNode(Void, IdentifierNode "hlloe", [], VariableDeclarationsNode [], BodyNode []) |]
      [| FunctionDeclarationNode(Void, IdentifierNode "asdlfkaslfji", [], VariableDeclarationsNode [], BodyNode []) |] ]

[<Theory>]
[<MemberData(nameof functionDefinitions)>]
let ``defineFunction always succeeds on empty context with program name other than the function name`` funcDecl =
    let ctx =
        SemanticContext.init (
            match funcDecl with
            | FunctionDeclarationNode(_, IdentifierNode name, _, _, _) -> name + "_program"
        )

    test <@ ctx |> SemanticContext.defineFunction funcDecl |> Result.isOk @>

[<Theory>]
[<MemberData(nameof functionDefinitions)>]
let ``defineFunction always succeeds on empty context with different program name even when redefining the same function``
    funcDecl
    =
    let ctx =
        SemanticContext.init (
            match funcDecl with
            | FunctionDeclarationNode(_, IdentifierNode name, _, _, _) -> name + "_program"
        )

    test
        <@
            ctx
            |> SemanticContext.defineFunction funcDecl
            |> Result.bind (SemanticContext.defineFunction funcDecl)
            |> Result.isOk
        @>

[<Theory>]
[<MemberData(nameof functionDefinitions)>]
let ``defineFunction always fails on empty context that has the same program name as the function name`` funcDecl =
    let ctx =
        SemanticContext.init (
            match funcDecl with
            | FunctionDeclarationNode(_, IdentifierNode name, _, _, _) -> name
        )

    test <@ ctx |> SemanticContext.defineFunction funcDecl |> Result.isError @>

let argumentDeclarations: obj[] list =
    [ [| ArgumentNode(IdentifierNode "Hello", IntType) |]
      [| ArgumentNode(IdentifierNode "World", FloatType) |]
      [| ArgumentNode(IdentifierNode "Test", BooleanType) |]
      [| ArgumentNode(IdentifierNode "Sample", StringType) |]
      [| ArgumentNode(IdentifierNode "Example", IntType) |]
      [| ArgumentNode(IdentifierNode "Data", FloatType) |]
      [| ArgumentNode(IdentifierNode "Random", BooleanType) |] ]


[<Theory>]
[<MemberData(nameof argumentDeclarations)>]
let ``defineArgument always succeeds on empty context with program name other than the argument name`` argDecl =
    let ctx =
        SemanticContext.init (
            match argDecl with
            | ArgumentNode(IdentifierNode name, _) -> name + "_program"
        )

    test <@ ctx |> SemanticContext.defineArgument argDecl |> Result.isOk @>

[<Theory>]
[<MemberData(nameof argumentDeclarations)>]
let ``defineArgument always succeeds on empty context with different program name even when redefining the same argument``
    argDecl
    =
    let ctx =
        SemanticContext.init (
            match argDecl with
            | ArgumentNode(IdentifierNode name, _) -> name + "_program"
        )

    test
        <@
            ctx
            |> SemanticContext.defineArgument argDecl
            |> Result.bind (SemanticContext.defineArgument argDecl)
            |> Result.isOk
        @>

[<Theory>]
[<MemberData(nameof argumentDeclarations)>]
let ``defineArgument always fails on empty context that has the same program name as the argument name`` argDecl =
    let ctx =
        SemanticContext.init (
            match argDecl with
            | ArgumentNode(IdentifierNode name, _) -> name
        )

    test <@ ctx |> SemanticContext.defineArgument argDecl |> Result.isError @>


let mixedDeclarations =
    List.zip3 variableDefinitions functionDefinitions argumentDeclarations
    |> List.map (fun (x: obj[], y: obj[], z: obj[]) -> Array.concat [ x; y; z ])

[<Theory>]
[<MemberData(nameof mixedDeclarations)>]
let ``defining a variable, a function, and an argument with a name different to the program should always succeed``
    varName
    varType
    funcDecl
    argDecl
    =
    let programName =
        match funcDecl, argDecl with
        | FunctionDeclarationNode(_, IdentifierNode funcName, _, _, _), ArgumentNode(IdentifierNode argName, _) ->
            "TestProgram_" + varName + "_" + funcName + "_" + argName

    let ctx = SemanticContext.init programName

    test
        <@
            ctx
            |> SemanticContext.defineVariable varType varName
            |> Result.bind (fun ctx -> SemanticContext.defineFunction funcDecl ctx)
            |> Result.bind (fun ctx -> SemanticContext.defineArgument argDecl ctx)
            |> Result.isOk
        @>


[<Theory>]
[<MemberData(nameof mixedDeclarations)>]
let ``getSymbol correctly resolves variable, function, and argument names after they are declared correctly`` varName varType funcDecl argDecl =
    let programName =
        match funcDecl, argDecl with
        | FunctionDeclarationNode(_, IdentifierNode funcName, _, _, _), ArgumentNode(IdentifierNode argName, _) ->
            "TestProgram_" + varName + "_" + funcName + "_" + argName

    let ctx =
        SemanticContext.init programName
        |> SemanticContext.defineVariable varType varName
        |> Result.bind (fun ctx -> SemanticContext.defineFunction funcDecl ctx)
        |> Result.bind (fun ctx -> SemanticContext.defineArgument argDecl ctx)
        |> Result.get

    test <@ varName |> SemanticContext.getSymbol ctx |> Result.isOk @>

    match funcDecl with
    | FunctionDeclarationNode(_, IdentifierNode funcName, _, _, _) ->
        test <@ funcName |> SemanticContext.getSymbol ctx |> Result.isOk @>

    match argDecl with
    | ArgumentNode(IdentifierNode argName, _) ->
        test <@ argName |> SemanticContext.getSymbol ctx |> Result.isOk @>


open System

[<Theory>]
[<MemberData(nameof mixedDeclarations)>]
let ``getSymbol should always fail after defining symbols if we get different names`` varName varType funcDecl argDecl =
    let programName =
        match funcDecl, argDecl with
        | FunctionDeclarationNode(_, IdentifierNode funcName, _, _, _), ArgumentNode(IdentifierNode argName, _) ->
            "TestProgram_" + varName + "_" + funcName + "_" + argName

    let ctx =
        SemanticContext.init programName
        |> SemanticContext.defineVariable varType varName
        |> Result.bind (fun ctx -> SemanticContext.defineFunction funcDecl ctx)
        |> Result.bind (fun ctx -> SemanticContext.defineArgument argDecl ctx)
        |> Result.get

    let addRandomSuffix name = name + "_" + Guid.NewGuid().ToString("N")

    test <@ addRandomSuffix varName |> SemanticContext.getSymbol ctx |> Result.isError @>

    match funcDecl with
    | FunctionDeclarationNode(_, IdentifierNode funcName, _, _, _) ->
        test <@ addRandomSuffix funcName |> SemanticContext.getSymbol ctx |> Result.isError @>

    match argDecl with
    | ArgumentNode(IdentifierNode argName, _) ->
        test <@ addRandomSuffix argName |> SemanticContext.getSymbol ctx |> Result.isError @>
