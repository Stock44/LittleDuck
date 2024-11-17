module LittleDuck.Tests.TypeResolvers

open FSharpPlus
open FSharpPlus.Data

open Xunit
open Swensen.Unquote

open LittleDuck

let valuesWithTypes: obj[] list =
    [ [| Float 2.3; FloatType |]
      [| Int 8; IntType |]
      [| Boolean false; BooleanType |]
      [| String "asd"; StringType |]
      [| Int 123456789; IntType |]
      [| Boolean true; BooleanType |]
      [| String "hello world"; StringType |]
      [| Float 3.14159; FloatType |]
      [| Int 0; IntType |]
      [| Boolean false; BooleanType |]
      [| String "test string"; StringType |] ]


[<Theory>]
[<MemberData(nameof valuesWithTypes)>]
let ``should always be able of resolving a value node's type correctly`` node expectedType =
    test <@ Resolve.valueType node = expectedType @>

let buildTestCtx programName varType varName =
    SemanticContext.init programName
    |> SemanticContext.defineVariable varType varName
    |> Result.get

let cteNodesWithContextAndExpectedType: obj[] list =
    [ [| NegativeCTE(Float 2.3); SemanticContext.init "program"; FloatType |]
      [| PositiveCTE(Int 3); SemanticContext.init "program"; IntType |]
      [| ValueCTE(Boolean false); SemanticContext.init "program"; BooleanType |]
      [| IdentifierCTE(IdentifierNode "test")
         buildTestCtx "program" BooleanType "test"
         BooleanType |]
      [| IdentifierCTE(IdentifierNode "amogus")
         buildTestCtx "program" StringType "amogus"
         StringType |]
      [| NegativeCTE(Float -0.1); SemanticContext.init "program"; FloatType |]
      [| PositiveCTE(Int 42); SemanticContext.init "program"; IntType |]
      [| ValueCTE(String "new string"); SemanticContext.init "program"; StringType |]
      [| IdentifierCTE(IdentifierNode "variable1")
         buildTestCtx "program" IntType "variable1"
         IntType |]
      [| IdentifierCTE(IdentifierNode "flag")
         buildTestCtx "program" BooleanType "flag"
         BooleanType |]
      [| NegativeCTE(Int -12345); SemanticContext.init "program"; IntType |]
      [| ValueCTE(Float 0.12345); SemanticContext.init "program"; FloatType |]
      [| IdentifierCTE(IdentifierNode "myString")
         buildTestCtx "program" StringType "myString"
         StringType |]
      [| PositiveCTE(Float 99.99); SemanticContext.init "program"; FloatType |]
      [| ValueCTE(Int 1000000); SemanticContext.init "program"; IntType |] ]

[<Theory>]
[<MemberData(nameof cteNodesWithContextAndExpectedType)>]
let ``should always resolve a cteType correctly, if a referenced variable is defined`` node ctx expectedType =
    let result = ReaderT.run (Resolve.cteType node) ctx
    test <@ result |> Result.get = expectedType @>


