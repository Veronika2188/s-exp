sealed class SExp {
    override fun toString(): String {
        return "SExp"
    }
}

class Atom(val value: String) : SExp() {
    override fun toString(): String {
        return value
    }
}

class SList(val children: List<SExp>) : SExp() {
    override fun toString(): String {
        return "(" + children.joinToString("") + ")"
    }
}

fun tokenize(input: String): List<String> {
    return input.replace("(", " ( ").replace(")", " ) ").split(" ").filter { it.isNotEmpty() }
}

fun parse(input: String): SExp {
    val tokens = tokenize(input)
    return parseTokens(tokens)
}

fun parseTokens(tokens: MutableList<String>): SExp {
    if (tokens.isEmpty()) {
        throw IllegalArgumentException("Unexpected end of input")
    }

    val token = tokens.removeAt(0)
    if (token == "(") {
        val children = mutableListOf<SExp>()
        while (tokens.isNotEmpty() && tokens[0] != ")") {
            children.add(parseTokens(tokens))
        }
        if (tokens.isEmpty()) {
            throw IllegalArgumentException("Unmatched (")
        }
        tokens.removeAt(0) // Remove ")"
        return SList(children)
    } else if (token == ")") {
        throw IllegalArgumentException("Unmatched )")
    } else {
        return Atom(token)
    }
}

fun main() {
    val input = "(+ 1 (* 2 3))"
    val result = parse(input)
    println(result)
}
