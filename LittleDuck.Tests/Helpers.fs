[<AutoOpen>]
module LittleDuck.Tests.Helpers

open Swensen.Unquote
open FParsec

module ParserResult =
    let unwrap =
        function
        | Success(value, _, _) -> value
        | Failure(fullError, _, _) -> failwith fullError

    let isSuccess =
        function
        | Success _ -> true
        | _ -> false


    let isFailure =
        function
        | Failure _ -> true
        | _ -> false

let testParserSuccess parser program =
    let result = program |> run (parser .>> eof)

    test <@ result |> ParserResult.isSuccess @>

let testParserFails parser program =
    let result = program |> run (parser .>> eof)

    test <@ result |> ParserResult.isFailure @>

let testParserEquals parser expected program =
    let result = program |> run (parser .>> eof)

    test <@ result |> ParserResult.isSuccess @>

    let output = result |> ParserResult.unwrap

    test <@ output = expected @>
