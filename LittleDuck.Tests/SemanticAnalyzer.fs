module LittleDuck.Tests.SemanticAnalyzer

open FSharpPlus.Control
open LittleDuck.Ast
open LittleDuck.SemanticAnalyzer
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can unwrap an identifier node`` () =
    test <@ IdentifierNode("loremIpsum") |> IdentifierTable.unwrapIdentifier = "loremIpsum" @>

[<Fact>]
let ``Can unwrap variable declaration and generate a list with all the same type`` () =
    let declaration =
        VariableDeclarationNode(
            [ IdentifierNode("lorem")
              IdentifierNode("ipsum")
              IdentifierNode("hawk")
              IdentifierNode("tuah") ],
            FloatType
        )

    test
        <@
            declaration |> IdentifierTable.unwrapVariableDeclaration = [ ("lorem", FloatType)
                                                                         ("ipsum", FloatType)
                                                                         ("hawk", FloatType)
                                                                         ("tuah", FloatType) ]
        @>


[<Fact>]
let ``Variable table is correctly generated for an empty variable declarations node`` () =
    let result =
        IdentifierTable.addVariableDeclarations (VariableDeclarationsNode([])) Map.empty

    test <@ result = Ok(Map([])) @>


[<Fact>]
let ``Variable table is correctly generated for a variable declarations node containing two declarations`` () =
    let result =
        IdentifierTable.addVariableDeclarations
            (VariableDeclarationsNode(
                [ VariableDeclarationNode([ IdentifierNode("lorem") ], FloatType)
                  VariableDeclarationNode([ IdentifierNode("ipsum") ], FloatType) ]
            ))
            Map.empty

    test <@ result = Ok(Map([ ("lorem", Variable(FloatType)); ("ipsum", Variable(FloatType)) ])) @>


[<Fact>]
let ``Variable table generation fails for repeated variable names in a single declaration`` () =
    let result =
        IdentifierTable.addVariableDeclarations
            (VariableDeclarationsNode(
                [ VariableDeclarationNode([ IdentifierNode("lorem"); IdentifierNode("lorem") ], FloatType) ]
            ))
            Map.empty

    test <@ result |> Result.isError @>

[<Fact>]
let ``Variable table generation fails for repeated variable names across multiple declarations`` () =
    let result =
        IdentifierTable.addVariableDeclarations
            (VariableDeclarationsNode(
                [ VariableDeclarationNode([ IdentifierNode("lorem") ], FloatType)
                  VariableDeclarationNode([ IdentifierNode("lorem") ], FloatType) ]
            ))
            Map.empty

    test <@ result |> Result.isError @>
