sealed trait SExp {
  def toString: String
}

case class Atom(value: String) extends SExp {
  override def toString: String = value
}

case class SList(children: List[SExp]) extends SExp {
  override def toString: String = "(" + children.map(_.toString).mkString + ")"
}

object Parser {
  def tokenize(input: String): List[String] = {
    input.replace("(", " ( ").replace(")", " ) ").split(" ").filter(_.nonEmpty).toList
  }

  def parseTokens(tokens: List[String]): (SExp, List[String]) = {
    if (tokens.isEmpty) {
      throw new IllegalArgumentException("Unexpected end of input")
    }

    val token = tokens.head
    val rest = tokens.tail
    token match {
      case "(" =>
        val (children, remaining) = parseList(rest)
        (SList(children), remaining)
      case ")" =>
        throw new IllegalArgumentException("Unmatched )")
      case _ =>
        (Atom(token), rest)
    }
  }

  def parseList(tokens: List[String]): (List[SExp], List[String]) = {
    if (tokens.isEmpty) {
      throw new IllegalArgumentException("Unmatched (")
    }
    if (tokens.head == ")") {
      (List(), tokens.tail)
    } else {
      val (sexp, remaining) = parseTokens(tokens)
      val (sexps, finalRemaining) = parseList(remaining)
      (sexp :: sexps, finalRemaining)
    }
  }

  def parse(input: String): SExp = {
    val tokens = tokenize(input)
    val (sexp, remaining) = parseTokens(tokens)
    if (remaining.isEmpty) {
      sexp
    } else {
      throw new IllegalArgumentException("Invalid S-expression")
    }
  }

  def main(args: Array[String]): Unit = {
    val input = "(+ 1 (* 2 3))"
    val result = parse(input)
    println(result)
  }
}
