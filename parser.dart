class SExp {
  @override
  String toString() {
    return 'SExp';
  }
}

class Atom extends SExp {
  final String value;
  Atom(this.value);

  @override
  String toString() {
    return value;
  }
}

class SList extends SExp {
  final List<SExp> children;
  SList(this.children);

  @override
  String toString() {
    return children.toString();
  }
}

/// Parses a string into an S-expression.
SExp parse(String input) {
  final tokens = tokenize(input);
  return parseTokens(tokens);
}

/// Tokenizes an input string into a list of tokens.
List<String> tokenize(String input) {
  return input
      .replaceAll('(', ' ( ')
      .replaceAll(')', ' ) ')
      .split(' ')
      .where((token) => token.isNotEmpty)
      .toList();
}

/// Parses a list of tokens into an S-expression.
SExp parseTokens(List<String> tokens) {
  if (tokens.isEmpty) {
    throw ArgumentError('Unexpected end of input');
  }

  final token = tokens.removeAt(0);
  if (token == '(') {
    final children = <SExp>[];
    while (tokens.isNotEmpty && tokens[0] != ')') {
      children.add(parseTokens(tokens));
    }
    if (tokens.isEmpty) {
      throw ArgumentError('Unmatched (');
    }
    tokens.removeAt(0); // Remove ')'
    return SList(children);
  } else if (token == ')') {
    throw ArgumentError('Unmatched )');
  } else {
    return Atom(token);
  }
}

void main() {
  const input = '(+ 1 (* 2 3))';
  final result = parse(input);
  print(result);
}
