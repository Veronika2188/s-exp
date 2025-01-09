# Pattern Matching

### What is Pattern Matching?

Pattern matching is a feature that allows you to match values against patterns and execute different code based on the match.

### Scala

**Example:**

```scala
token match {
  case "(" =>
    val (children, remaining) = parseList(rest)
    (SList(children), remaining)
  case ")" =>
    throw new IllegalArgumentException("Unmatched )")
  case _ =>
    (Atom(token), rest)
}
```

### Swift

**Example:**

```swift
switch token {
case "(":
    return try parseList(tokens: &tokens)
case ")":
    throw ParserError.unmatchedClosingParenthesis
default:
    return Atom(value: token)
}
```

### Haskell

**Example:**

```haskell
stringOfSExp :: SExp -> String
stringOfSExp sexp =
    case sexp of
    | Atom v -> v
    | SList l -> "(" <> concatMap show l <> ")"
```

-   Here, `stringOfSExp` uses pattern matching to handle different cases of the `SExp` type.

### OCaml

**Example:**

```ocaml
let rec string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"
```

-   Here, `string_of_sexp` uses pattern matching to handle different cases of the `sexp` type.

### Rust

**Example:**

```rust
impl SExp {
    fn to_string(&self) -> String {
        match self {
            SExp::Atom(v) => v.clone(),
            SExp::SList(l) => {
                let mut result = String::from("(");
                for sexp in l {
                    result.push_str(&sexp.to_string());
                }
                result.push_str(")");
                result
            }
        }
    }
}
```

-   Here, `match` is used to handle different cases of the `SExp` enum.

### C++

C++ does not have direct support for pattern matching, but you can use `if` statements or `switch` statements.

**Example:**

```cpp
SExp* parseTokens(std::vector<std::string>& tokens) {
    if (tokens.empty()) {
        throw std::invalid_argument("Unexpected end of input");
    }

    std::string token = tokens[0];
    tokens.erase(tokens.begin());
    if (token == "(") {
        std::vector<SExp*> children;
        while (!tokens.empty() && tokens[0] != ")") {
            children.push_back(parseTokens(tokens));
        }
        if (tokens.empty()) {
            throw std::invalid_argument("Unmatched (");
        }
        tokens.erase(tokens.begin()); // Remove ")"
        return new SList(children);
    } else if (token == ")") {
        throw std::invalid_argument("Unmatched )");
    } else {
        return new Atom(token);
    }
}
```

-   Here, `if` statements are used to handle different cases of the input.

### C#

C# uses `switch` statements for pattern matching.

**Example:**

```csharp
public static SExp ParseTokens(List<string> tokens)
{
    if (tokens.Count == 0)
    {
        throw new ArgumentException("Unexpected end of input");
    }

    string token = tokens[0];
    tokens.RemoveAt(0);
    switch (token)
    {
        case "(":
            List<SExp> children = new List<SExp>();
            while (tokens.Count > 0 && tokens[0] != ")")
            {
                children.Add(ParseTokens(tokens));
            }
            if (tokens.Count == 0)
            {
                throw new ArgumentException("Unmatched (");
            }
            tokens.RemoveAt(0); // Remove ")"
            return new SList(children);
        case ")":
            throw new ArgumentException("Unmatched )");
        default:
            return new Atom(token);
    }
}
```

-   Here, a `switch` statement is used to handle different cases of the input.

### Dart

Dart uses `if` statements and `switch` statements for pattern matching.

**Example:**

```dart
SExp parseTokens(List<String> tokens) {
  if (tokens.isEmpty) {
    throw Exception("Unexpected end of input");
  }

  String token = tokens.removeAt(0);
  if (token == "(") {
    List<SExp> children = [];
    while (tokens.isNotEmpty && tokens[0] != ")") {
      children.add(parseTokens(tokens));
    }
    if (tokens.isEmpty) {
      throw Exception("Unmatched (");
    }
    tokens.removeAt(0); // Remove ")"
    return SList(children);
  } else if (token == ")") {
    throw Exception("Unmatched )");
  } else {
    return Atom(token);
  }
}
```

-   Here, `if` statements are used to handle different cases of the input.

### Kotlin

Kotlin uses `when` expressions for pattern matching.

**Example:**

```kotlin
fun parseTokens(tokens: MutableList<String>): SExp {
    if (tokens.isEmpty()) {
        throw IllegalArgumentException("Unmatched (")
    }

    val token = tokens.removeAt(0)
    return when (token) {
        "(" -> {
            val children = mutableListOf<SExp>()
            while (tokens.isNotEmpty() && tokens[0] != ")") {
                children.add(parseTokens(tokens))
            }
            if (tokens.isEmpty()) {
                throw IllegalArgumentException("Unmatched (")
            }
            tokens.removeAt(0) // Remove ")"
            SList(children)
        }
        ")" -> throw IllegalArgumentException("Unmatched )")
        else -> Atom(token)
    }
}
```

-   Here, a `when` expression is used to handle different cases of the input.

### Python

Python uses `if/elif/else` statements for pattern matching.

**Example:**

```python
def parse_tokens(tokens):
    if not tokens:
        raise ValueError("Unexpected end of input")

    token = tokens.pop(0)
    if token == "(":
        children = []
        while tokens and tokens[0] != ")":
            children.append(parse_tokens(tokens))
        if not tokens:
            raise ValueError("Unmatched (")
        tokens.pop(0)  # Remove ")"
        return SList(children)
    elif token == ")":
        raise ValueError("Unmatched )")
    else:
        return Atom(token)
```

-   Here, `if/elif/else` statements are used to handle different cases of the input.

### Go

Go uses `if` statements and `switch` statements for pattern matching.

**Example:**

```go
func parseTokens(tokens []string) (SExp, []string, error) {
	if len(tokens) == 0 {
		return nil, tokens, errors.New("Unexpected end of input")
	}

	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		children := []SExp{}
		for len(tokens) > 0 && tokens[0] != ")" {
			child, rest, err := parseTokens(tokens)
			if err != nil {
				return nil, tokens, err
			}
			children = append(children, child)
			tokens = rest
		}
		if len(tokens) == 0 {
			return nil, tokens, errors.New("Unmatched (")
		}
		tokens = tokens[1:] // Remove ")"
		return SList{children: children}, tokens, nil
	case ")":
		return nil, tokens, errors.New("Unmatched )")
	default:
		return Atom{value: token}, tokens, nil
	}
}
```

-   Here, a `switch` statement is used to handle different cases of the input.

### Lisp

Lisp uses `cond` for pattern matching.

**Example:**

```lisp
(defun parse-tokens (tokens)
  (if (null tokens)
      (error "Unexpected end of input")
      (let ((token (pop tokens)))
        (cond
          ((string= token "(")
           (let ((children '()))
             (loop while (and tokens (not (string= (car tokens) ")")))
                   do (push (parse-tokens tokens) children))
             (if (null tokens)
                 (error "Unmatched (")
                 (pop tokens))
             (make-sexp :type 'list :value (reverse children))))
          ((string= token ")")
           (error "Unmatched )"))
          (t (make-sexp :type 'atom :value token))))))
```

-   Here, `cond` is used to handle different cases of the input.

### Clojure

Clojure uses `cond` for pattern matching.

**Example:**

```clojure
(defn parse-tokens [tokens]
  (if (empty? tokens)
      (throw (Exception. "Unexpected end of input"))
      (let [token (first tokens)
            rest (rest tokens)]
        (cond
          (= token "(")
          (let [[children remaining] (parse-list rest)]
            [(make-list children) remaining])
          (= token ")")
          (throw (Exception. "Unmatched )"))
          :else
          [(make-atom token) rest]))))
```

-   Here, `cond` is used to handle different cases of the input.

### F#

F# uses `match` expressions for pattern matching.

**Example:**

```fsharp
let rec parseTokens tokens =
    match tokens with
    | [] -> raise (Failure "Unexpected end of input")
    | token :: rest ->
        match token with
        | "(" ->
            let children, rest = parseList rest
            SList children, rest
        | ")" -> raise (Failure "Unmatched )")
        | _ -> Atom token, rest
and parseList tokens =
    match tokens with
    | [] -> raise (Failure "Unmatched (")
    | ")" :: rest -> [], rest
    | _ ->
        let sexp, rest = parseTokens tokens
        let sexps, finalRest = parseList rest
        sexp :: sexps, finalRest
```

-   Here, `match` expressions are used to handle different cases of the input.
</content>
</replace_in_file>
```
# Pattern Matching

### What is Pattern Matching?

Pattern matching is a feature that allows you to match values against patterns and execute different code based on the match.

### Scala

**Example:**

```scala
token match {
  case "(" =>
    val (children, remaining) = parseList(rest)
    (SList(children), remaining)
  case ")" =>
    throw new IllegalArgumentException("Unmatched )")
  case _ =>
    (Atom(token), rest)
}
```

### Swift

**Example:**

```swift
switch token {
case "(":
    return try parseList(tokens: &tokens)
case ")":
    throw ParserError.unmatchedClosingParenthesis
default:
    return Atom(value: token)
}
```

### Haskell

**Example:**

```haskell
stringOfSExp :: SExp -> String
stringOfSExp sexp =
    case sexp of
    | Atom v -> v
    | SList l -> "(" <> concatMap show l <> ")"
```

-   Here, `stringOfSExp` uses pattern matching to handle different cases of the `SExp` type.

### OCaml

**Example:**

```ocaml
let rec string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"
```

-   Here, `string_of_sexp` uses pattern matching to handle different cases of the `sexp` type.

### Rust

**Example:**

```rust
impl SExp {
    fn to_string(&self) -> String {
        match self {
            SExp::Atom(v) => v.clone(),
            SExp::SList(l) => {
                let mut result = String::from("(");
                for sexp in l {
                    result.push_str(&sexp.to_string());
                }
                result.push_str(")");
                result
            }
        }
    }
}
```

-   Here, `match` is used to handle different cases of the `SExp` enum.
