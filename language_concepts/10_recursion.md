# Recursion

### What is Recursion?

Recursion is a technique where a function calls itself to solve a problem.

### Clojure

**Example:**

```clojure
(defn parse-list [tokens]
  (cond
    (empty? tokens) (throw (Exception. "Unmatched ("))
    (= (first tokens) ")") [[] (rest tokens)]
    :else
    (let [[sexp remaining] (parse-tokens tokens)
          [sexps final-remaining] (parse-list remaining)]
      [(cons sexp sexps) final-remaining])))
```

-   Here, `parse-list` is a recursive function that calls itself to parse the list of tokens.

### Haskell

**Example:**

```haskell
parseList :: [String] -> ([SExp], [String])
parseList tokens = case tokens of
  -- If the list is empty, throw an error
  [] -> error "Unmatched ("
  -- If the next token is a closing parenthesis, return an empty list and the remaining tokens
  ")":rest -> ([], rest)
  -- Otherwise, parse an S-expression and recursively parse the rest of the list
  _ -> do
        let (sexp, rest) = parseTokens' tokens
        case parseList rest of
            (sexps, finalRest) -> (sexp:sexps, finalRest)
```

-   Here, `parseList` is a recursive function that calls itself to parse the list of tokens.

### OCaml

**Example:**

```ocaml
let rec parse_tokens tokens =
  match tokens with
  | [] -> raise (Failure "Unexpected end of input")
  | token :: rest ->
    match token with
    | "(" ->
      let children, rest = parse_list rest in
      SList children, rest
    | ")" -> raise (Failure "Unmatched )")
    | _ -> Atom token, rest
```

-   Here, `parse_tokens` is a recursive function that calls itself to parse the list of tokens.

### C++

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

-   Here, `parseTokens` is a recursive function that calls itself to parse the list of tokens.

### C#

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
    if (token == "(")
    {
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
    }
    else if (token == ")")
    {
        throw new ArgumentException("Unmatched )");
    }
    else
    {
        return new Atom(token);
    }
}
```

-   Here, `ParseTokens` is a recursive function that calls itself to parse the list of tokens.

### Dart

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

-   Here, `parseTokens` is a recursive function that calls itself to parse the list of tokens.

### Kotlin

**Example:**

```kotlin
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
```

-   Here, `parseTokens` is a recursive function that calls itself to parse the list of tokens.

### Python

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

-   Here, `parse_tokens` is a recursive function that calls itself to parse the list of tokens.

### Go

**Example:**

```go
func parseTokens(tokens []string) (SExp, []string, error) {
	if len(tokens) == 0 {
		return nil, tokens, errors.New("Unexpected end of input")
	}

	token := tokens[0]
	tokens = tokens[1:]
	if token == "(" {
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
	} else if token == ")" {
		return nil, tokens, errors.New("Unmatched )")
	} else {
		return Atom{value: token}, tokens, nil
	}
}
```

-   Here, `parseTokens` is a recursive function that calls itself to parse the list of tokens.

### Rust

**Example:**

```rust
fn parse_tokens(tokens: &mut Vec<String>) -> Result<SExp, String> {
    if tokens.is_empty() {
        return Err("Unexpected end of input".to_string());
    }

    let token = tokens.remove(0);
    if token == "(" {
        let mut children = Vec::new();
        while !tokens.is_empty() && tokens[0] != ")" {
            children.push(parse_tokens(tokens)?);
        }
        if tokens.is_empty() {
            return Err("Unmatched (".to_string());
        }
        tokens.remove(0); // Remove ")"
        return Ok(SExp::SList(children));
    } else if token == ")" {
        return Err("Unmatched )".to_string());
    } else {
        return Ok(SExp::Atom(token));
    }
}
```

-   Here, `parse_tokens` is a recursive function that calls itself to parse the list of tokens.

### Lisp

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

-   Here, `parse-tokens` is a recursive function that calls itself to parse the list of tokens.

### Clojure

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

-   Here, `parse-tokens` is a recursive function that calls itself to parse the list of tokens.

### F#

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

-   Here, `parseTokens` and `parseList` are recursive functions that call each other to parse the list of tokens.
</content>
</replace_in_file>
```
# Recursion

### What is Recursion?

Recursion is a technique where a function calls itself to solve a problem.

### Clojure

**Example:**

```clojure
(defn parse-list [tokens]
  (cond
    (empty? tokens) (throw (Exception. "Unmatched ("))
    (= (first tokens) ")") [[] (rest tokens)]
    :else
    (let [[sexp remaining] (parse-tokens tokens)
          [sexps final-remaining] (parse-list remaining)]
      [(cons sexp sexps) final-remaining])))
```

-   Here, `parse-list` is a recursive function that calls itself to parse the list of tokens.

### Haskell

**Example:**

```haskell
parseList :: [String] -> ([SExp], [String])
parseList tokens = case tokens of
  -- If the list is empty, throw an error
  [] -> error "Unmatched ("
  -- If the next token is a closing parenthesis, return an empty list and the remaining tokens
  ")":rest -> ([], rest)
  -- Otherwise, parse an S-expression and recursively parse the rest of the list
  _ -> do
        let (sexp, rest) = parseTokens' tokens
        case parseList rest of
            (sexps, finalRest) -> (sexp:sexps, finalRest)
```

-   Here, `parseList` is a recursive function that calls itself to parse the list of tokens.

### OCaml

**Example:**

```ocaml
let rec parse_tokens tokens =
  match tokens with
  | [] -> raise (Failure "Unexpected end of input")
  | token :: rest ->
    match token with
    | "(" ->
      let children, rest = parse_list rest in
      SList children, rest
    | ")" -> raise (Failure "Unmatched )")
    | _ -> Atom token, rest
```

-   Here, `parse_tokens` is a recursive function that calls itself to parse the list of tokens.
