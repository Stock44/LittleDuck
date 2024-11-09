# LittleDuck

## Description

This project is a sample F# compiler for the LittleDuck language, targeting .NET 8.0. It is implemented
with the [FParsec](https://www.quanttec.com/fparsec/) parser combinator library.

## Features

- Parsing of the LittleDuck language.
- Inline generation of the language's AST.
- **In progress:** Semantic analyzer for the AST.

## Prerequisites

- [.NET 8.0 SDK](https://dotnet.microsoft.com/download/dotnet/8.0)

## Installation

1. Clone the repository:
   ```sh
   git clone https://github.com/Stock44/LittleDuck.git
   ```
2. Navigate to the project directory:
   ```sh
   cd project-name
   ```
3. Restore dependencies:
   ```sh
   dotnet restore
   ```

## Solution structure

The solution is divided into two projects: `LittleDuck` and `LittleDuck.Tests`. The first one contains
the full implementation of the compiler, while the second one contains the test suite.

## Usage

The main executable is not yet configured to parse and compile. For now all testing of the compiler
is done via the test suite, available via the following command:

```sh
dotnet test
```

## License

This project is licensed under the MIT License—see the [LICENSE](LICENSE) file for details.
