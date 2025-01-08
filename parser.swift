// parser.swift

import Foundation

protocol SExp {
    func toString() -> String
}

class Atom: SExp {
    let value: String
    init(value: String) {
        self.value = value
    }
    func toString() -> String {
        return value
    }
}

class SList: SExp {
    let children: [SExp]
    init(children: [SExp]) {
        self.children = children
    }
    func toString() -> String {
        return "(" + children.map { $0.toString() }.joined() + ")"
    }
}

class Parser {
    func tokenize(input: String) -> [String] {
        return input.replacingOccurrences(of: "(", with: " ( ").replacingOccurrences(of: ")", with: " ) ").components(separatedBy: " ").filter { !$0.isEmpty }
    }

    func parseTokens(tokens: inout [String]) throws -> SExp {
        if tokens.isEmpty {
            throw ParserError.unexpectedEndOfInput
        }

        let token = tokens.removeFirst()
        switch token {
        case "(":
            return try parseList(tokens: &tokens)
        case ")":
            throw ParserError.unmatchedClosingParenthesis
        default:
            return Atom(value: token)
        }
    }

    func parseList(tokens: inout [String]) throws -> SExp {
        var children: [SExp] = []
        while !tokens.isEmpty && tokens[0] != ")" {
            children.append(try parseTokens(tokens: &tokens))
        }
        if tokens.isEmpty {
            throw ParserError.unmatchedOpeningParenthesis
        }
        tokens.removeFirst() // Remove ")"
        return SList(children: children)
    }

    func parse(input: String) throws -> SExp {
        var tokens = tokenize(input: input)
        let result = try parseTokens(tokens: &tokens)
        if !tokens.isEmpty {
            throw ParserError.invalidSExpression
        }
        return result
    }
}

enum ParserError: Error {
    case unexpectedEndOfInput
    case unmatchedClosingParenthesis
    case unmatchedOpeningParenthesis
    case invalidSExpression
}

let parser = Parser()
do {
    let result = try parser.parse(input: "(+ 1 (* 2 3))")
    print(result.toString())
} catch {
    print("Error: \(error)")
}
