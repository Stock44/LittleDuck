module LittleDuck.Tests.IntegrationTests

open LittleDuck
open Xunit
open FSharpPlus
open Swensen.Unquote

[<AutoOpen>]
module Helpers =
    let isInvocationArgumentTypeMismatch =
        function
        | InvocationArgumentTypeMismatch _ -> true
        | _ -> false

    let isInvalidConditionType =
        function
        | InvalidConditionType _ -> true
        | _ -> false

    let isReturnTypeMismatch =
        function
        | ReturnTypeMismatch _ -> true
        | _ -> false

    let isInvalidBinaryOperandTypes =
        function
        | InvalidBinaryOperandTypes _ -> true
        | _ -> false

    let isAssignmentWithWrongType =
        function
        | AssignmentWithWrongType _ -> true
        | _ -> false

    let isInvokedProgram =
        function
        | InvokedProgram -> true
        | _ -> false

    let isInvokedVariable =
        function
        | InvokedVariable _ -> true
        | _ -> false

    let isInvokedArgument =
        function
        | InvokedArgument _ -> true
        | _ -> false

let correctProgram =
    """program test;

x: int;

void foo() [
{
    x = 5;
}
];

void bar(a: int) [
    {
        if (a > 0) {
            x = a;
        };
    }
];

main {
}

end
"""

[<Fact>]
let ``sample program should be parsed correctly, and analysis should end in no errors.`` () =
    let analysis =
        correctProgram
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    test <@ analysis.Errors.Length = 0 @>

let programWithUsageError =
    """program test;

x: int;

void foo() [
{
    x = 5;
}
];

void bar(a: int) [
    {
        if (a > 0) {
            x = a;
        };
    }
];

main {
    x = false; // Error: cannot assign bool to int
}

end
"""

[<Fact>]
let ``sample program with assignment error be parsed correctly, and analysis should end in one AssignmentWithWrongType error.``
    ()
    =
    let analysis =
        programWithUsageError
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    test <@ analysis.Errors.Length = 1 @>

    test
        <@
            match analysis.Errors[0] with
            | AssignmentWithWrongType _ -> true
            | _ -> false
        @>

let programWithTwoErrors =
    """program test;

numberVar: int;

☺: string;

int sumInts(a: int, b: int) [
    {
        return a + b;
    }
];

main {
    if (sumInts(2.4 /* error: cannot use float as an input to an int argument */, 2) /* cannot use int function as boolean condition */) {
        return;
    }

    print(☺);
}

end
"""

[<Fact>]
let ``sample program with two semantic errors should be parsed correctly, and analysis should end in two error.`` () =
    let analysis =
        programWithTwoErrors
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    test <@ analysis.Errors.Length = 2 @>

    test <@ isInvalidConditionType analysis.Errors[0] @>
    test <@ isInvocationArgumentTypeMismatch analysis.Errors[1] @>

let programWithThreeErrors =
    """program test;

numberVar: int;

☺: string;

int sumInts(a: int, b: int) [
    {
        if (a > b) {
            return a + b;
        }

        return false; // Cannot return boolean from int-returning function
    }
];

main {
    if (sumInts(4, 2) /* Cannot use int value as boolean in condition */) {
        return;
    }

    print(☺ / "asdf"); // Cannot divide an int by a string
}

end
"""

[<Fact>]
let ``sample program with three errors should result in analysis with three errors`` () =
    let analysis =
        programWithThreeErrors
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    let errors = analysis.Errors

    test <@ errors.Length = 3 @>

    let error0 = analysis.Errors[0]
    let error1 = analysis.Errors[1]
    let error2 = analysis.Errors[2]

    test <@ isReturnTypeMismatch error2 @>

    test <@ isInvalidConditionType error1 @>

    test <@ isInvalidBinaryOperandTypes error0 @>

let programWithComplexErrors =
    """program test;

numberVar: int;

str: string;

int divide(a: int, b: int) [
    {
        if (b == 0) {
            return false; // Cannot return boolean from int-returning function
        }
        return a / b;
    }
];

main {
    numberVar = divide(10, 0);
    str = numberVar; // Cannot assign number to string variable
    print(str + numberVar); // Cannot sum string and number
}

end
"""

[<Fact>]
let ``sample program with complex errors should result in analysis with four errors`` () =
    let analysis =
        programWithComplexErrors
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    let errors = analysis.Errors

    test <@ errors.Length = 3 @>

    let error0 = analysis.Errors[0]
    let error1 = analysis.Errors[1]
    let error2 = analysis.Errors[2]

    test <@ isReturnTypeMismatch error2 @>
    test <@ isAssignmentWithWrongType error1 @>
    test <@ isInvalidBinaryOperandTypes error0 @>


let programWithManyErrors =
    """program test;

var1: int;
var2: int;
str1: string;
str2: string;

void foo(a: int) [
    {
        str1 = a;                   // Error 1: Type mismatch
    }
];

int bar(a: int, b: string) [
    {
        return a + b;               // Error 2: Operand type mismatch
    }
];

void baz() [
    {
        var1 = "string";            // Error 3: Type mismatch
    }
];

main {
    foo("not an int");              // Error 4: Argument type mismatch
    var2 = bar(1, 2);               // Error 5: Argument type mismatch
    print(str1 / str2);             // Error 6: Invalid binary operation
    var1 = str2 + var1;             // Error 7: Binary operation type mismatch
}

end
"""

[<Fact>]
let ``sample program with many errors should result in analysis with seven errors`` () =
    let analysis =
        programWithManyErrors
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    let errors = analysis.Errors

    test <@ errors.Length = 7 @>

    let error0 = errors[0]
    let error1 = errors[1]
    let error2 = errors[2]
    let error3 = errors[3]
    let error4 = errors[4]
    let error5 = errors[5]
    let error6 = errors[6]

    test <@ isAssignmentWithWrongType error6 @>
    test <@ isInvalidBinaryOperandTypes error5 @>
    test <@ isAssignmentWithWrongType error4 @>
    test <@ isInvocationArgumentTypeMismatch error3 @>
    test <@ isInvocationArgumentTypeMismatch error2 @>
    test <@ isInvalidBinaryOperandTypes error1 @>
    test <@ isInvalidBinaryOperandTypes error0 @>


let programWithVariableArgument =
    """program test;

    x: int;
    x_copy: int;

    int identity(a: int) [
        {
            return a;
        }
    ];

    main  {
            x = 5;
            x_copy = identity(x);

            return ; // Correct usage
    }

    end
    """

[<Fact>]
let ``sample correct program should result in no errors`` () =
    let analysis =
        programWithVariableArgument
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    test <@ analysis.Errors.Length = 0 @>


let programWithInvocationErrors =
    """program test;

    x: int;

    main {
        x = 5;
        test(); // Error: Program invoked instead of method
        x();    // Error: Variable invoked as if it were a method
        return;
    }

    end
    """

[<Fact>]
let ``sample program with invocation errors should result in analysis with two errors`` () =
    let analysis =
        programWithInvocationErrors
        |> LittleDuck.parse
        |> Result.get
        |> SemanticAnalysis.processProgram

    let errors = analysis.Errors

    test <@ errors.Length = 2 @>

    let error0 = errors[0]
    let error1 = errors[1]

    test <@ isInvokedProgram error1 @>
    test <@ isInvokedVariable error0 @>
