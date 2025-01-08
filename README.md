# S-Expression Parser

This project contains implementations of a simple S-expression parser in various programming languages.

## Algorithm

The parsers in this project use a recursive descent parsing algorithm. The algorithm can be broken down into the following steps:

1.  **Tokenization:** The input string is tokenized into a list of tokens. This involves adding spaces around parentheses and then splitting the string by spaces.
2.  **Parsing:** The list of tokens is parsed into an S-expression. This is done using a recursive descent parser. The parser uses the following rules:
    -   If the current token is an opening parenthesis `(`, then parse a list of S-expressions until a closing parenthesis `)` is encountered.
    -   If the current token is a closing parenthesis `)`, then return from the current parsing context.
    -   If the current token is anything else, then it is an atom.

## Implementations

The following implementations are included:

-   Haskell: `parser.hs` - Uses a recursive descent parser with pattern matching. The parser defines a data type `SExp` to represent S-expressions, and uses functions like `tokenize`, `parseTokens`, and `parseList` to parse the input string.
-   JavaScript: `parser.js` - Uses a recursive descent parser with classes for `SExp`, `Atom`, and `SList`. The `SExp` class is a base class, and `Atom` and `SList` are derived classes. The parser uses functions like `tokenize`, `parseTokensHelper`, and `parseList` to parse the input string.
-   Dart: `parser.dart` - Uses classes for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` class is a base class, and `Atom` and `SList` are derived classes. The parser uses functions like `tokenize` and `parseTokens` to parse the input string.
-   Java: `Parser.java` - Uses classes for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` class is a base class, and `Atom` and `SList` are derived classes. The parser uses methods like `tokenize` and `parseTokens` to parse the input string.
-   Python: `parser.py` - Uses classes for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` class is a base class, and `Atom` and `SList` are derived classes. The parser uses functions like `tokenize` and `parse_tokens` to parse the input string.
-   Kotlin: `Parser.kt` - Uses classes for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` class is a sealed class, and `Atom` and `SList` are derived classes. The parser uses functions like `tokenize` and `parseTokens` to parse the input string.
-   C#: `Parser.cs` - Uses classes for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` class is an abstract class, and `Atom` and `SList` are derived classes. The parser uses methods like `Tokenize` and `ParseTokens` to parse the input string.
-   C++: `parser.cpp` - Uses classes for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` class is a base class, and `Atom` and `SList` are derived classes. The parser uses functions like `tokenize` and `parseTokens` to parse the input string.
-   C: `parser.c` - Uses structs for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The parser uses functions like `tokenize` and `parse_tokens` to parse the input string.
-   OCaml: `parser.ml` - Uses a recursive descent parser with pattern matching. The parser defines a data type `sexp` to represent S-expressions, and uses functions like `tokenize`, `parse_tokens`, and `parse_list` to parse the input string.
-   F#: `parser.fs` - Uses a recursive descent parser with pattern matching. The parser defines a type `SExp` to represent S-expressions, and uses functions like `tokenize`, `parseTokens`, and `parseList` to parse the input string.
-   Go: `parser.go` - Uses structs and interfaces for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` interface is implemented by `Atom` and `SList` structs. The parser uses functions like `tokenize` and `parseTokens` to parse the input string.
-   Rust: `parser.rs` - Uses enums for `SExp`, `Atom`, and `SList` with a recursive descent parsing approach. The `SExp` enum represents either an `Atom` or an `SList`. The parser uses functions like `tokenize` and `parse_tokens` to parse the input string.
-   Common Lisp: `parser.lisp` - Uses `defstruct` for `SExp` and recursive functions for parsing. The parser uses functions like `tokenize`, `parse-tokens`, and `parse-list` to parse the input string.
-   Scheme: `parser.scm` - Uses lists and recursive functions for parsing. The parser uses functions like `tokenize`, `parse-tokens`, and `parse-list` to parse the input string.
-   Clojure: `parser.clj` - Uses maps and recursive functions for parsing. The parser uses functions like `tokenize`, `parse-tokens`, and `parse-list` to parse the input string.
-   Swift: `parser.swift` - Uses classes for `Atom` and `SList` conforming to the `SExp` protocol with a recursive descent parsing approach. The parser uses functions like `tokenize`, `parseTokens`, and `parseList` to parse the input string.
