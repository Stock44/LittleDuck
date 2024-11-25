module LittleDuck.Tests.Parser

open FParsec
open Xunit
open Swensen.Unquote

open LittleDuck.Ast
open LittleDuck.Parser

module ``Identifier parsing`` =
    let validIdentifiers: obj[] list =
        [ [| "hello" |]
          [| "wowee" |]
          [| "fjao9291" |]
          [| "HelloWorld123" |]
          [| "SCREAMING_SNAKE_CASE" |]
          [| "height_dm" |]
          [| "length_µm" |]
          [| "reul" |]
          [| "ə" |]
          [| "x" |]
          [| "λ" |]
          [| "γλώσσα" |]
          [| "🔥" |]
          [| "😂" |]
          [| "✨" |]
          [| "簡體版" |]
          [| "簡體版239ocHaha" |]
          [| "TestCase可通過每頁左上角的連結隨時調整" |]
          [| "TheQuickBrownFoxJumpedOverTheLazyFox" |] ]


    let invalidIdentifiers: obj[] list =
        [ [| "2hello" |]
          [| "wowee-asdf" |]
          [| "fjao9291/asdf" |]
          [| "erick/" |]
          [| "HelloWorld*123" |]
          [| "SCREAMING_SNAKE_[CASE" |]
          [| "height-dm()" |]
          [| "leng{th_µm" |]
          [| "ə\"" |]
          [| "λ,sdf" |]
          [| "γλώ.σσα" |]
          [| "🔥+Hi" |]
          [| "😂*+" |]
          [| "✨<" |]
          [| "簡>體版" |]
          [| "簡!體版239ocHaha" |]
          [| "Test=Case可通過每頁左上角的連結隨時調整" |]
          [| "TheQuickBrownFoxJumpedOverTheLazyFox**" |] ]

    [<Theory>]
    [<MemberData(nameof validIdentifiers)>]
    let ``Identifier that contains only valid characters succeeds in parsing`` identifier =
        identifier |> testParserSuccess Identifier.parse

    [<Theory>]
    [<MemberData(nameof invalidIdentifiers)>]
    let ``Identifier that contains illegal characters should fail while parsing`` identifier =
        identifier |> testParserFails Identifier.parse

module ``Value parsing`` =
    let validValues: obj[] list =
        [ [| "12.5"; Float 12.5 |]
          [| "28.8"; Float 28.8 |]
          [| "06.9"; Float 6.9 |]
          [| "923.2"; Float 923.2 |]
          [| "8"; Int 8 |]
          [| "3"; Int 3 |]
          [| "128498"; Int 128498 |]
          [| "999.2189239127"; Float 999.2189239127 |]
          [| "89218471827.21"; Float 89218471827.21 |]
          [| "12.0"; Float 12.0 |]
          [| "8."; Float 8. |]
          [| "0.00021"; Float 0.00021 |]
          [| "0.0102"; Float 0.0102 |]
          [| "\"Hello world!\""; String "Hello world!" |]
          [| "\"☺\""; String "☺" |]
          [| "\"\""; String "" |]
          [| "\" \""; String " " |]
          [| "\"    \""; String "    " |]
          [| "\"8.22\""; String "8.22" |]
          [| "false"; Boolean false |]
          [| "true"; Boolean true |] ]

    let invalidValues: obj[] list =
        [ [| "hl" |]
          [| "21.asdf" |]
          [| "21m" |]
          [| "933mdoa201" |]
          [| "0.221m" |]
          [| "-0.221" |]
          [| "-28" |]
          [| "0939x" |]
          [| "\"Hello" |]
          [| "World!\"" |] ]

    [<Theory>]
    [<MemberData(nameof validValues)>]
    let ``Value that contains only valid characters succeeds in parsing`` value _ =
        let result = value |> run (Value.parse .>> eof)
        test <@ result |> ParserResult.isSuccess @>

    [<Theory>]
    [<MemberData(nameof invalidValues)>]
    let ``Value that contains illegal characters should fail while parsing`` value =
        let result = value |> run (Value.parse .>> eof)
        test <@ result |> ParserResult.isFailure @>


    [<Theory>]
    [<MemberData(nameof validValues)>]
    let ``Values are correctly parsed to their CLI representations`` input value =
        input |> testParserEquals Value.parse value

module ``CTE parsing`` =
    let validNegativeCtes: obj[] list =
        [ [| "-28.32"; NegativeCTE(Float 28.32) |]
          [| "-4"; NegativeCTE(Int 4) |]
          [| "-  192.29"; NegativeCTE(Float 192.29) |]
          [| "- 0"; NegativeCTE(Int 0) |]
          [| "- 0.0"; NegativeCTE(Float 0) |]
          [| "- 29"; NegativeCTE(Int 29) |] ]


    let validPositveCtes: obj[] list =
        [ [| "+28.32"; PositiveCTE(Float 28.32) |]
          [| "+4"; PositiveCTE(Int 4) |]
          [| "+  192.29"; PositiveCTE(Float 192.29) |]
          [| "+ 0"; PositiveCTE(Int 0) |]
          [| "+ 0.0"; PositiveCTE(Float 0) |]
          [| "+ 29"; PositiveCTE(Int 29) |] ]


    let validValueCtes: obj[] list =
        [ [| "28.32"; ValueCTE(Float 28.32) |]
          [| "4"; ValueCTE(Int 4) |]
          [| "192.29"; ValueCTE(Float 192.29) |]
          [| "0"; ValueCTE(Int 0) |]
          [| "0.0"; ValueCTE(Float 0) |]
          [| "29"; ValueCTE(Int 29) |] ]

    let validIdentifierCtes: obj[] list =
        [ [| "hello"; IdentifierCTE(IdentifierNode("hello")) |]
          [| "wowee"; IdentifierCTE(IdentifierNode("wowee")) |]
          [| "fjao9291"; IdentifierCTE(IdentifierNode("fjao9291")) |]
          [| "HelloWorld123"; IdentifierCTE(IdentifierNode("HelloWorld123")) |]
          [| "SCREAMING_SNAKE_CASE"
             IdentifierCTE(IdentifierNode("SCREAMING_SNAKE_CASE")) |]
          [| "height_dm"; IdentifierCTE(IdentifierNode("height_dm")) |]
          [| "length_µm"; IdentifierCTE(IdentifierNode("length_µm")) |]
          [| "ə"; IdentifierCTE(IdentifierNode("ə")) |]
          [| "x"; IdentifierCTE(IdentifierNode("x")) |]
          [| "λ"; IdentifierCTE(IdentifierNode("λ")) |]
          [| "γλώσσα"; IdentifierCTE(IdentifierNode("γλώσσα")) |]
          [| "🔥"; IdentifierCTE(IdentifierNode("🔥")) |]
          [| "😂"; IdentifierCTE(IdentifierNode("😂")) |]
          [| "✨"; IdentifierCTE(IdentifierNode("✨")) |]
          [| "簡體版"; IdentifierCTE(IdentifierNode("簡體版")) |]
          [| "簡體版239ocHaha"; IdentifierCTE(IdentifierNode("簡體版239ocHaha")) |]
          [| "TestCase可通過每頁左上角的連結隨時調整"
             IdentifierCTE(IdentifierNode("TestCase可通過每頁左上角的連結隨時調整")) |]
          [| "TheQuickBrownFoxJumpedOverTheLazyFox"
             IdentifierCTE(IdentifierNode("TheQuickBrownFoxJumpedOverTheLazyFox")) |] ]


    [<Theory>]
    [<MemberData(nameof validNegativeCtes)>]
    let ``Negative CTEs are correctly parsed`` input expectedCTE =
        input |> testParserEquals CTE.parse expectedCTE


    [<Theory>]
    [<MemberData(nameof validPositveCtes)>]
    let ``Positive CTEs are correctly parsed`` input expectedCTE =
        input |> testParserEquals CTE.parse expectedCTE


    [<Theory>]
    [<MemberData(nameof validValueCtes)>]
    let ``Value CTEs are correctly parsed`` input expectedCTE =
        input |> testParserEquals CTE.parse expectedCTE

    [<Theory>]
    [<MemberData(nameof validIdentifierCtes)>]
    let ``Identifier CTEs are correctly parsed`` input expectedCTE =
        input |> testParserEquals CTE.parse expectedCTE

module ``Factor parsing`` =
    let makeParenthesized x =
        ParenthesizedExprFactor(ExpExpression(TermExp(FactorTerm(x))))


    let validParenthesizedExpressions: obj[] list =
        [ [| "(2)"; makeParenthesized (CTEFactor(ValueCTE(Int 2))) |]
          [| "(  2  )"; makeParenthesized (CTEFactor(ValueCTE(Int 2))) |]
          [| "(2.2)"; makeParenthesized (CTEFactor(ValueCTE(Float 2.2))) |]
          [| "(2.2  )"; makeParenthesized (CTEFactor(ValueCTE(Float 2.2))) |]
          [| "(    2.2  )"; makeParenthesized (CTEFactor(ValueCTE(Float 2.2))) |]
          [| "(counter)"
             makeParenthesized (CTEFactor(IdentifierCTE(IdentifierNode "counter"))) |]
          [| "(-4)"; makeParenthesized (CTEFactor(NegativeCTE(Int 4))) |]
          [| "(  -  4)"; makeParenthesized (CTEFactor(NegativeCTE(Int 4))) |]
          [| "(+4)"; makeParenthesized (CTEFactor(PositiveCTE(Int 4))) |]
          [| "(+  4  )"; makeParenthesized (CTEFactor(PositiveCTE(Int 4))) |]
          [| "((4))"
             makeParenthesized (makeParenthesized (CTEFactor(ValueCTE(Int 4)))) |]
          [| "( (4 ))"
             makeParenthesized (makeParenthesized (CTEFactor(ValueCTE(Int 4)))) |]
          [| "(((4)))"
             makeParenthesized (makeParenthesized (makeParenthesized (CTEFactor(ValueCTE(Int 4))))) |] ]

    [<Fact>]
    let ``Factor can wrap a valid CTE`` () =
        let result = "4.8" |> run (Factor.parse .>> eof)

        test <@ result |> ParserResult.isSuccess @>

    [<Fact>]
    let ``Factor can surround a CTE with parenthesis`` () =
        let result = "(4.8)" |> run (Factor.parse .>> eof)

        test <@ result |> ParserResult.isSuccess @>

    [<Theory>]
    [<MemberData(nameof validParenthesizedExpressions)>]
    let ``Parenthesized expressions factors are correctly parsed`` input expectedExpr =
        input |> testParserEquals Factor.parse expectedExpr

module ``Term parsing`` =
    [<Fact>]
    let ``Can parse "2 / 2" as a division term`` () =
        "2 / 2"
        |> testParserEquals
            Term.parse
            (DivisionTerm(FactorTerm(CTEFactor(ValueCTE(Int 2))), CTEFactor(ValueCTE(Int 2))))


    [<Fact>]
    let ``Can parse "2 / (4 * 2.5)" correctly, with parenthesis taking precedence `` () =
        "2 / (4 * 2.5)"
        |> testParserEquals
            Term.parse
            (DivisionTerm(
                FactorTerm(CTEFactor(ValueCTE(Int 2))),
                ParenthesizedExprFactor(
                    ExpExpression(
                        TermExp(
                            MultiplicationTerm(FactorTerm(CTEFactor(ValueCTE(Int 4))), CTEFactor(ValueCTE(Float 2.5)))
                        )
                    )
                )
            ))


    [<Fact>]
    let ``Can parse "2 * 3 / 7" correctly`` () =
        let result = "2 * 3 / 7" |> run (Term.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = DivisionTerm(
                    MultiplicationTerm(FactorTerm(CTEFactor(ValueCTE(Int 2))), CTEFactor(ValueCTE(Int 3))),
                    CTEFactor(ValueCTE(Int 7))
                )
            @>


    [<Fact>]
    let ``Can parse "2 * 3 * 4 / 7 / 8" correctly with left associativity`` () =
        let result = "2 *   3* 4 / 7/8" |> run (Term.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = DivisionTerm(
                    DivisionTerm(
                        MultiplicationTerm(
                            MultiplicationTerm(FactorTerm(CTEFactor(ValueCTE(Int 2))), CTEFactor(ValueCTE(Int 3))),
                            CTEFactor(ValueCTE(Int 4))
                        ),
                        CTEFactor(ValueCTE(Int 7))
                    ),
                    CTEFactor(ValueCTE(Int 8))
                )
            @>

module ``Exp parsing`` =
    [<Fact>]
    let ``Can parse "2 + 2" as a sum exp`` () =
        let result = "2 + 2" |> run (Exp.parse .>> eof) |> ParserResult.unwrap

        test
            <@ result = SumExp(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))), FactorTerm(CTEFactor(ValueCTE(Int 2)))) @>

    [<Fact>]
    let ``Can parse "2 - 2" as a subtraction exp`` () =
        let result = "2 - 2" |> run (Exp.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = SubtractionExp(
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))),
                    FactorTerm(CTEFactor(ValueCTE(Int 2)))
                )
            @>


    [<Fact>]
    let ``Can parse "2 * 3 + 4 / 7 " as a sum of a multiplication and a division`` () =
        let result = "2 * 3 + 4 / 7" |> run (Exp.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = SumExp(
                    TermExp(MultiplicationTerm(FactorTerm(CTEFactor(ValueCTE(Int 2))), CTEFactor(ValueCTE(Int 3)))),
                    DivisionTerm(FactorTerm(CTEFactor(ValueCTE(Int 4))), CTEFactor(ValueCTE(Int 7)))
                )
            @>


module ``Expression parsing`` =
    [<Fact>]
    let ``Can parse "2 < 5" as a less than expression`` () =
        let result = "2 < 5" |> run (Expression.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = LessThanExpression(
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))),
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 5))))
                )
            @>


    [<Fact>]
    let ``Can parse "2 > 5" as a more than expression`` () =
        let result = "2 > 5" |> run (Expression.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = GreaterThanExpression(
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))),
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 5))))
                )
            @>


    [<Fact>]
    let ``Can parse "2 != 5" as a not equal expression`` () =
        let result = "2 != 5" |> run (Expression.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = NotEqualExpression(
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))),
                    TermExp(FactorTerm(CTEFactor(ValueCTE(Int 5))))
                )
            @>

module ``Print parsing`` =
    [<Fact>]
    let ``Can parse print("hello") as a print statement`` () =
        let result = "print(\"hello\")" |> run (Print.parse .>> eof) |> ParserResult.unwrap

        test <@ result = PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("hello")))))) ]) @>

    [<Fact>]
    let ``Can parse print("hello", "world", "!") as print statement of a list of printables`` () =
        let result =
            "print(\"hello\", \"world\", \"!\")"
            |> run (Print.parse .>> eof)
            |> ParserResult.unwrap

        test
            <@
                result = PrintNode(
                    [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("hello"))))))
                      ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("world"))))))
                      ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("!")))))) ]
                )
            @>

module ``Invocation parsing`` =
    [<Fact>]
    let ``Can parse hello(12) as an invocation with one arg`` () =
        let result = "hello(12)" |> run (Invocation.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = InvocationNode(
                    IdentifierNode("hello"),
                    [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 12))))) ]
                )
            @>


    [<Fact>]
    let ``Can parse lorem_ipsum  (  12, 12.3, 92 ) as an invocation with three args`` () =
        let result =
            "lorem_ipsum  (  12, 12.3, 92 )"
            |> run (Invocation.parse .>> eof)
            |> ParserResult.unwrap

        test
            <@
                result = InvocationNode(
                    IdentifierNode("lorem_ipsum"),
                    [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 12)))))
                      ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Float 12.3)))))
                      ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 92))))) ]
                )
            @>

    [<Fact>]
    let ``Can parse ☺() as an invocation with no parameters`` () =
        let result = "☺()" |> run (Invocation.parse .>> eof) |> ParserResult.unwrap

        test <@ result = InvocationNode(IdentifierNode("☺"), []) @>

module ``Cycle parsing`` =
    [<Fact>]
    let ``Can parse a simple cycle`` () =
        let program =
            """while {
    print("Hello world!");
}
do (4 > 2)"""

        let result = program |> run (Cycle.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = CycleNode(
                    BodyNode(
                        [ PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          ) ]
                    ),
                    GreaterThanExpression(
                        TermExp(FactorTerm(CTEFactor(ValueCTE(Int 4)))),
                        TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))
                    )
                )
            @>


    [<Fact>]
    let ``Can parse a multi step cycle`` () =
        let program =
            """while {
    print("Hello world!");print("Hello world!");
    print("Hello world!");


    print("Hello world!");  print("Hello world!");print("Hello world!");
}
do (4 > 2)"""

        let result = program |> run (Cycle.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = CycleNode(
                    BodyNode(
                        [ PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          )
                          PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          )
                          PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          )
                          PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          )
                          PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          )
                          PrintStatement(
                              PrintNode(
                                  [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                              )
                          ) ]
                    ),
                    GreaterThanExpression(
                        TermExp(FactorTerm(CTEFactor(ValueCTE(Int 4)))),
                        TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))
                    )
                )
            @>

module ``If parsing`` =
    [<Fact>]
    let ``Can parse a simple if `` () =
        let program = """if(2){print("hi");}"""

        let result = program |> run (If.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = IfNode(
                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))),
                    BodyNode(
                        [ PrintStatement(
                              PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("hi")))))) ])
                          ) ]
                    )
                )
            @>


    [<Fact>]
    let ``Can parse an if with lots of whitespace `` () =
        let program =
            """if    (  2
)

{


    print    (    "hi"            )   ;
    }"""

        let result = program |> run (If.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = IfNode(
                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))),
                    BodyNode(
                        [ PrintStatement(
                              PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("hi")))))) ])
                          ) ]
                    )
                )
            @>

module ``Else parsing`` =

    [<Fact>]
    let ``Can parse a simple else `` () =
        let program = """else(2){print("hi");}"""

        let result = program |> run (Else.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = ElseNode(
                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))),
                    BodyNode(
                        [ PrintStatement(
                              PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("hi")))))) ])
                          ) ]
                    )
                )
            @>


    [<Fact>]
    let ``Can parse an else with lots of whitespace `` () =
        let program =
            """else    (  2
)

{


    print    (    "hi"            )   ;
    }"""

        let result = program |> run (Else.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = ElseNode(
                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))),
                    BodyNode(
                        [ PrintStatement(
                              PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("hi")))))) ])
                          ) ]
                    )
                )
            @>

module ``Condition parsing`` =
    let ``Can parse a condition with only an If `` () =
        let program = """if (2) {}"""

        let result = program |> run (Condition.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = IfCondition(
                    IfNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))), BodyNode([]))
                )
            @>

    let ``Can parse a condition with both an If and an Else`` () =
        let program = """if (2) {} else (4) {}"""

        let result = program |> run (Condition.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = IfElseCondition(
                    IfNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))), BodyNode([])),
                    ElseNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))), BodyNode([]))
                )
            @>

    let ``Can parse a condition with both an If and an Else and weird spacing`` () =
        let program =
            """if (2) {
        }


else (4  ) {
}
"""

        let result = program |> run (Condition.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = IfElseCondition(
                    IfNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))), BodyNode([])),
                    ElseNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))), BodyNode([]))
                )
            @>

module ``Assignment parsing`` =
    [<Fact>]
    let ``Can parse a simple assignment`` () =
        let program = "loremIpsum = 2"

        let result = program |> run (Assignment.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = AssignmentNode(
                    IdentifierNode("loremIpsum"),
                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))))
                )
            @>

    [<Fact>]
    let ``Can parse an assignment with weird spacing`` () =
        let program =
            "loremIpsum

        =     2"

        let result = program |> run (Assignment.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = AssignmentNode(
                    IdentifierNode("loremIpsum"),
                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))))
                )
            @>

module ``Statement parsing`` =
    [<Fact>]
    let ``Can parse a print statement`` () =
        let program = """print("Hello world!");"""

        let result =
            program |> run (Statement.parse .>> eof) |> ParserResult.unwrap |> Option.get

        test
            <@
                result = PrintStatement(
                    PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ])
                )
            @>

    [<Fact>]
    let ``Can parse a cycle statement`` () =
        let program =
            """while {
    print("Hello world!");
}
do (4 > 2)"""

        let result =
            program |> run (Statement.parse .>> eof) |> ParserResult.unwrap |> Option.get

        test
            <@
                result = CycleStatement(
                    CycleNode(
                        BodyNode(
                            [ PrintStatement(
                                  PrintNode(
                                      [ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ]
                                  )
                              ) ]
                        ),
                        GreaterThanExpression(
                            TermExp(FactorTerm(CTEFactor(ValueCTE(Int 4)))),
                            TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))
                        )
                    )
                )
            @>

    [<Fact>]
    let ``Can parse an invocation statement`` () =
        let program = """invoke();"""

        let result =
            program |> run (Statement.parse .>> eof) |> ParserResult.unwrap |> Option.get

        test <@ result = InvocationStatement(InvocationNode(IdentifierNode("invoke"), [])) @>

    [<Fact>]
    let ``Can parse a condition statement`` () =
        let program = """if (2) {} else (4) {}"""

        let result =
            program |> run (Statement.parse .>> eof) |> ParserResult.unwrap |> Option.get

        test
            <@
                result = ConditionStatement(
                    IfElseCondition(
                        IfNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))), BodyNode([])),
                        ElseNode(ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 4))))), BodyNode([]))
                    )
                )
            @>

    [<Fact>]
    let ``Can parse an assignment statement`` () =
        let program = "loremIpsum = 2;"

        let result =
            program |> run (Statement.parse .>> eof) |> ParserResult.unwrap |> Option.get

        test
            <@
                result = AssignmentStatement(
                    AssignmentNode(
                        IdentifierNode("loremIpsum"),
                        ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))))
                    )
                )
            @>


module ``Body parsing`` =

    [<Fact>]
    let ``Can parse an empty body`` () =
        let program = "{}"

        let result = program |> run (Body.parse .>> eof) |> ParserResult.unwrap

        test <@ result = BodyNode([]) @>

    [<Fact>]
    let ``Can parse a body with a single statement`` () =
        let program = "{print(\"Hello world!\");}"

        let result = program |> run (Body.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = BodyNode(
                    [ PrintStatement(
                          PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ])
                      ) ]
                )
            @>

    [<Fact>]
    let ``Can parse a body with multiple statements`` () =
        let program = "{loremIpsum = 2; print(\"Hello world!\"); invoke();}"

        let result = program |> run (Body.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = BodyNode(
                    [ AssignmentStatement(
                          AssignmentNode(
                              IdentifierNode("loremIpsum"),
                              ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2)))))
                          )
                      )
                      PrintStatement(
                          PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello world!")))))) ])
                      )
                      InvocationStatement(InvocationNode(IdentifierNode("invoke"), [])) ]
                )
            @>

    [<Fact>]
    let ``Can parse a body with nested statements`` () =
        let program = "{if (2) { print(\"Hello\"); } else (4) { print(\"World\"); };}"

        let result = program |> run (Body.parse .>> eof) |> ParserResult.unwrap

        test
            <@
                result = BodyNode(
                    [ ConditionStatement(
                          IfElseCondition(
                              IfNode(
                                  ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 2))))),
                                  BodyNode(
                                      [ PrintStatement(
                                            PrintNode(
                                                [ ExpExpression(
                                                      TermExp(FactorTerm(CTEFactor(ValueCTE(String("Hello")))))
                                                  ) ]
                                            )
                                        ) ]
                                  )
                              ),
                              ElseNode(
                                  ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 4))))),
                                  BodyNode(
                                      [ PrintStatement(
                                            PrintNode(
                                                [ ExpExpression(
                                                      TermExp(FactorTerm(CTEFactor(ValueCTE(String("World")))))
                                                  ) ]
                                            )
                                        ) ]
                                  )
                              )
                          )
                      ) ]
                )
            @>

module ``Type parsing`` =
    [<Fact>]
    let ``Can parse the int type`` () =
        "int" |> testParserEquals Type.parse IntType

    [<Fact>]
    let ``Can parse the float type`` () =
        "float" |> testParserEquals Type.parse FloatType


    [<Fact>]
    let ``Can parse the boolean type`` () =
        "bool" |> testParserEquals Type.parse BooleanType


    [<Fact>]
    let ``Can parse the string type`` () =
        "string" |> testParserEquals Type.parse StringType

module ``VariableDeclaration parsing`` =
    [<Fact>]
    let ``Can parse a simple variable declaration`` () =
        "loremIpsum: int"
        |> testParserEquals
            VariableDeclaration.parse
            (VariableDeclarationNode([ IdentifierNode("loremIpsum") ], IntType))

    [<Fact>]
    let ``Can parse a multi variable declaration`` () =
        "loremIpsum, amogus, ☺, tecnológicoDeMonterrey, °: int"
        |> testParserEquals
            VariableDeclaration.parse
            (VariableDeclarationNode(
                [ IdentifierNode("loremIpsum")
                  IdentifierNode("amogus")
                  IdentifierNode("☺")
                  IdentifierNode("tecnológicoDeMonterrey")
                  IdentifierNode("°") ],
                IntType
            ))


    [<Fact>]
    let ``Can parse a multi variable, weirdly spaced declaration`` () =
        "loremIpsum
            ,


            amogus,     ☺
            ,

                          tecnológicoDeMonterrey,
                                           °    :
                                           int"
        |> testParserEquals
            VariableDeclaration.parse
            (VariableDeclarationNode(
                [ IdentifierNode("loremIpsum")
                  IdentifierNode("amogus")
                  IdentifierNode("☺")
                  IdentifierNode("tecnológicoDeMonterrey")
                  IdentifierNode("°") ],
                IntType
            ))

module ``VariableDeclarations parsing`` =
    [<Fact>]
    let ``Can parse two variable declarations`` () =
        "lorem: int; ipsum: float;"
        |> testParserEquals
            VariableDeclarations.parse
            (VariableDeclarationsNode(
                [ VariableDeclarationNode([ IdentifierNode("lorem") ], IntType)
                  VariableDeclarationNode([ IdentifierNode("ipsum") ], FloatType) ]
            ))


    [<Fact>]
    let ``Can parse a variable declarations that is empty`` () =
        "" |> testParserEquals VariableDeclarations.parse (VariableDeclarationsNode([]))

    [<Fact>]
    let ``Can parse two variable declarations and one multi variable declaration`` () =
        "lorem: int; ipsum: float; loremIpsum, amogus, ☺, tecnológicoDeMonterrey, °: int;"
        |> testParserEquals
            VariableDeclarations.parse
            (VariableDeclarationsNode(
                [ VariableDeclarationNode([ IdentifierNode("lorem") ], IntType)
                  VariableDeclarationNode([ IdentifierNode("ipsum") ], FloatType)
                  VariableDeclarationNode(
                      [ IdentifierNode("loremIpsum")
                        IdentifierNode("amogus")
                        IdentifierNode("☺")
                        IdentifierNode("tecnológicoDeMonterrey")
                        IdentifierNode("°") ],
                      IntType
                  ) ]
            ))


    [<Fact>]
    let ``Can parse two variable declarations and one multi variable declaration with weird spacing`` () =
        "lorem: int;
        ipsum: float;
             loremIpsum  , amogus, ☺    , tecnológicoDeMonterrey, °
             : int
             ;"
        |> testParserEquals
            VariableDeclarations.parse
            (VariableDeclarationsNode(
                [ VariableDeclarationNode([ IdentifierNode("lorem") ], IntType)
                  VariableDeclarationNode([ IdentifierNode("ipsum") ], FloatType)
                  VariableDeclarationNode(
                      [ IdentifierNode("loremIpsum")
                        IdentifierNode("amogus")
                        IdentifierNode("☺")
                        IdentifierNode("tecnológicoDeMonterrey")
                        IdentifierNode("°") ],
                      IntType
                  ) ]
            ))

module ``Argument parsing`` =
    [<Fact>]
    let ``Can parse a simple float argument`` () =
        "loremIpsum: float"
        |> testParserEquals Argument.parse (ArgumentNode(IdentifierNode("loremIpsum"), FloatType))

    [<Fact>]
    let ``Can parse a simple int argument`` () =
        "loremIpsum:int"
        |> testParserEquals Argument.parse (ArgumentNode(IdentifierNode("loremIpsum"), IntType))

module ``FunctionDeclaration parsing`` =
    [<Fact>]
    let ``Can parse an argument list of two elements`` () =
        "lorem: float, ipsum: int"
        |> testParserEquals
            FunctionDeclaration.pArgumentList
            [ ArgumentNode(IdentifierNode("lorem"), FloatType)
              ArgumentNode(IdentifierNode("ipsum"), IntType) ]


    [<Fact>]
    let ``Can parse an argument list of three elements with weird spacing`` () =
        "lorem
        :float,ipsum
        :
        int,           dolor:
        float"
        |> testParserEquals
            FunctionDeclaration.pArgumentList
            [ ArgumentNode(IdentifierNode("lorem"), FloatType)
              ArgumentNode(IdentifierNode("ipsum"), IntType)
              ArgumentNode(IdentifierNode("dolor"), FloatType) ]

    [<Fact>]
    let ``Can parse a no-argument function declaration`` () =
        "void amogus()[{}]"
        |> testParserEquals
            FunctionDeclaration.parse
            (FunctionDeclarationNode(Void, IdentifierNode("amogus"), [], VariableDeclarationsNode([]), BodyNode([])))


    [<Fact>]
    let ``Can parse a multi-argument function declaration`` () =
        "void amogus(lorem: float, ipsum: int)[{}]"
        |> testParserEquals
            FunctionDeclaration.parse
            (FunctionDeclarationNode(
                Void,
                IdentifierNode("amogus"),
                [ ArgumentNode(IdentifierNode("lorem"), FloatType)
                  ArgumentNode(IdentifierNode("ipsum"), IntType) ],
                VariableDeclarationsNode([]),
                BodyNode([])
            ))


    [<Fact>]
    let ``Can parse a multi-argument, with scoped variable declarations, function declaration`` () =
        "void amogus(lorem: float, ipsum: int)[dolor:float;sitAmet:int;{}]"
        |> testParserEquals
            FunctionDeclaration.parse
            (FunctionDeclarationNode(
                Void,
                IdentifierNode("amogus"),
                [ ArgumentNode(IdentifierNode("lorem"), FloatType)
                  ArgumentNode(IdentifierNode("ipsum"), IntType) ],
                VariableDeclarationsNode(
                    [ VariableDeclarationNode([ IdentifierNode("dolor") ], FloatType)
                      VariableDeclarationNode([ IdentifierNode("sitAmet") ], IntType) ]
                ),
                BodyNode([])
            ))


    [<Fact>]
    let ``Can parse a multi-argument, with scoped variable declarations, function declaration with a print in the body``
        ()
        =
        "void amogus(lorem: float, ipsum: int)[dolor:float;sitAmet:int;{print(\"amogus\");}]"
        |> testParserEquals
            FunctionDeclaration.parse
            (FunctionDeclarationNode(
                Void,
                IdentifierNode("amogus"),
                [ ArgumentNode(IdentifierNode("lorem"), FloatType)
                  ArgumentNode(IdentifierNode("ipsum"), IntType) ],
                VariableDeclarationsNode(
                    [ VariableDeclarationNode([ IdentifierNode("dolor") ], FloatType)
                      VariableDeclarationNode([ IdentifierNode("sitAmet") ], IntType) ]
                ),
                BodyNode(
                    [ PrintStatement(
                          PrintNode([ ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(String("amogus")))))) ])
                      ) ]
                )
            ))

module ``FunctionDeclarations parsing`` =
    [<Fact>]
    let ``Can parse two function declarations`` () =
        "void lorem()[{}]; void ipsum()[{}];"
        |> testParserEquals
            FunctionDeclarations.parse
            (FunctionDeclarationsNode(
                [ FunctionDeclarationNode(Void, IdentifierNode("lorem"), [], VariableDeclarationsNode([]), BodyNode([]))
                  FunctionDeclarationNode(Void, IdentifierNode("ipsum"), [], VariableDeclarationsNode([]), BodyNode([])) ]
            ))

    [<Fact>]
    let ``Can parse two function declarations with arguments`` () =
        "void lorem(a: int, b: float)[{}]; void ipsum(c: float)[{}];"
        |> testParserEquals
            FunctionDeclarations.parse
            (FunctionDeclarationsNode(
                [ FunctionDeclarationNode(
                      Void,
                      IdentifierNode("lorem"),
                      [ ArgumentNode(IdentifierNode("a"), IntType)
                        ArgumentNode(IdentifierNode("b"), FloatType) ],
                      VariableDeclarationsNode([]),
                      BodyNode([])
                  )
                  FunctionDeclarationNode(
                      Void,
                      IdentifierNode("ipsum"),
                      [ ArgumentNode(IdentifierNode("c"), FloatType) ],
                      VariableDeclarationsNode([]),
                      BodyNode([])
                  ) ]
            ))

    [<Fact>]
    let ``Can parse three function declarations with one having a complex body`` () =
        "void lorem()[{}]; void ipsum()[x: int; {x = 0;}]; void dolor(a: float, b: int)[{}];"
        |> testParserEquals
            FunctionDeclarations.parse
            (FunctionDeclarationsNode(
                [ FunctionDeclarationNode(Void, IdentifierNode("lorem"), [], VariableDeclarationsNode([]), BodyNode([]))
                  FunctionDeclarationNode(
                      Void,
                      IdentifierNode("ipsum"),
                      [],
                      VariableDeclarationsNode([ VariableDeclarationNode([ IdentifierNode("x") ], IntType) ]),
                      BodyNode(
                          [ AssignmentStatement(
                                AssignmentNode(
                                    IdentifierNode("x"),
                                    ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 0)))))
                                )
                            ) ]
                      )
                  )
                  FunctionDeclarationNode(
                      Void,
                      IdentifierNode("dolor"),
                      [ ArgumentNode(IdentifierNode("a"), FloatType)
                        ArgumentNode(IdentifierNode("b"), IntType) ],
                      VariableDeclarationsNode([]),
                      BodyNode([])
                  ) ]
            ))

module ``Program parsing`` =
    [<Fact>]
    let ``Can parse a program with no variables, no functions, and no body `` () =
        "program amogus; main {} end"
        |> testParserEquals
            Program.parse
            (ProgramNode(
                IdentifierNode("amogus"),
                VariableDeclarationsNode([]),
                FunctionDeclarationsNode([]),
                BodyNode([])
            ))


    [<Fact>]
    let ``Can parse a program with one variable and no functions`` () =
        "program test; x: int; main {} end"
        |> testParserEquals
            Program.parse
            (ProgramNode(
                IdentifierNode("test"),
                VariableDeclarationsNode([ VariableDeclarationNode([ IdentifierNode("x") ], IntType) ]),
                FunctionDeclarationsNode([]),
                BodyNode([])
            ))

    [<Fact>]
    let ``Can parse a program with multiple variables and no functions`` () =
        "program test; x: int; y: float; main { return x; } end"
        |> testParserEquals
            Program.parse
            (ProgramNode(
                IdentifierNode("test"),
                VariableDeclarationsNode(
                    [ VariableDeclarationNode([ IdentifierNode("x") ], IntType)
                      VariableDeclarationNode([ IdentifierNode("y") ], FloatType) ]
                ),
                FunctionDeclarationsNode([]),
                BodyNode(
                    [ ReturnStatement(
                          ReturnNode(
                              Some(ExpExpression(TermExp(FactorTerm(CTEFactor(IdentifierCTE(IdentifierNode("x")))))))
                          )
                      ) ]
                )
            ))

    [<Fact>]
    let ``Can parse a program with one function and no variables`` () =
        "program test; void foo()[{ return; }]; main {} end"
        |> testParserEquals
            Program.parse
            (ProgramNode(
                IdentifierNode("test"),
                VariableDeclarationsNode([]),
                FunctionDeclarationsNode(
                    [ FunctionDeclarationNode(
                          Void,
                          IdentifierNode("foo"),
                          [],
                          VariableDeclarationsNode([]),
                          BodyNode([ ReturnStatement(ReturnNode None) ])
                      ) ]
                ),
                BodyNode([])
            ))

    [<Fact>]
    let ``Can parse a program with variables and functions`` () =
        "program test; x: int; y: float; void foo()[{}]; void bar(a: int)[{}]; main {  } end"
        |> testParserEquals
            Program.parse
            (ProgramNode(
                IdentifierNode("test"),
                VariableDeclarationsNode(
                    [ VariableDeclarationNode([ IdentifierNode("x") ], IntType)
                      VariableDeclarationNode([ IdentifierNode("y") ], FloatType) ]
                ),
                FunctionDeclarationsNode(
                    [ FunctionDeclarationNode(
                          Void,
                          IdentifierNode("foo"),
                          [],
                          VariableDeclarationsNode([]),
                          BodyNode([])
                      )
                      FunctionDeclarationNode(
                          Void,
                          IdentifierNode("bar"),
                          [ ArgumentNode(IdentifierNode("a"), IntType) ],
                          VariableDeclarationsNode([]),
                          BodyNode([])
                      ) ]
                ),
                BodyNode([])
            ))

    [<Fact>]
    let ``Can parse a program with complex function bodies`` () =
        "program test;

         x: int;

         int foo()[{x = 5;}];
         float bar(a: int) [
            {
                if (a > 0) {
                    x = a;
                };
            }
         ];

         main {
         }
         end"

        |> testParserEquals
            Program.parse
            (ProgramNode(
                IdentifierNode("test"),
                VariableDeclarationsNode([ VariableDeclarationNode([ IdentifierNode("x") ], IntType) ]),
                FunctionDeclarationsNode(
                    [ FunctionDeclarationNode(
                          ReturnType(IntType),
                          IdentifierNode("foo"),
                          [],
                          VariableDeclarationsNode([]),
                          BodyNode(
                              [ AssignmentStatement(
                                    AssignmentNode(
                                        IdentifierNode("x"),
                                        ExpExpression(TermExp(FactorTerm(CTEFactor(ValueCTE(Int 5)))))
                                    )
                                ) ]
                          )
                      )
                      FunctionDeclarationNode(
                          ReturnType(FloatType),
                          IdentifierNode("bar"),
                          [ ArgumentNode(IdentifierNode("a"), IntType) ],
                          VariableDeclarationsNode([]),
                          BodyNode(
                              [ ConditionStatement(
                                    IfCondition(
                                        IfNode(
                                            GreaterThanExpression(
                                                TermExp(FactorTerm(CTEFactor(IdentifierCTE(IdentifierNode("a"))))),
                                                TermExp(FactorTerm(CTEFactor(ValueCTE(Int 0))))
                                            ),
                                            BodyNode(
                                                [ AssignmentStatement(
                                                      AssignmentNode(
                                                          IdentifierNode("x"),
                                                          ExpExpression(
                                                              TermExp(
                                                                  FactorTerm(
                                                                      CTEFactor(IdentifierCTE(IdentifierNode("a")))
                                                                  )
                                                              )
                                                          )
                                                      )
                                                  ) ]
                                            )
                                        )
                                    )
                                ) ]
                          )
                      ) ]
                ),
                BodyNode([])
            ))
