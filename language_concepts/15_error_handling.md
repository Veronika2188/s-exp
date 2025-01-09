# Error Handling (Rust)

### What is Error Handling (Rust)?

Error handling in Rust uses the `Result` type to represent either a successful value or an error.

### Rust

**Example:**

```rust
fn parse_tokens(tokens: &mut Vec<String>) -> Result<SExp, String> {
    if tokens.is_empty() {
        return Err("Unexpected end of input".to_string());
    }
    ...
}
```

-   Here, `Result<SExp, String>` is used to represent either a successful `SExp` or an error message.

### C++

C++ uses exceptions for error handling.

**Example:**

```cpp
SExp* parseTokens(std::vector<std::string>& tokens) {
    if (tokens.empty()) {
        throw std::invalid_argument("Unexpected end of input");
    }
    ...
}
```

-   Here, exceptions are used to handle errors.

### C#

C# uses exceptions for error handling.

**Example:**

```csharp
public static SExp ParseTokens(List<string> tokens)
{
    if (tokens.Count == 0)
    {
        throw new ArgumentException("Unexpected end of input");
    }
    ...
}
```

-   Here, exceptions are used to handle errors.

### Dart

Dart uses exceptions for error handling.

**Example:**

```dart
SExp parseTokens(List<String> tokens) {
  if (tokens.isEmpty) {
    throw Exception("Unexpected end of input");
  }
  ...
}
```

-   Here, exceptions are used to handle errors.

### Kotlin

Kotlin uses exceptions for error handling.

**Example:**

```kotlin
fun parseTokens(tokens: MutableList<String>): SExp {
    if (tokens.isEmpty()) {
        throw IllegalArgumentException("Unexpected end of input")
    }
    ...
}
```

-   Here, exceptions are used to handle errors.

### Python

Python uses exceptions for error handling.

**Example:**

```python
def parse_tokens(tokens):
    if not tokens:
        raise ValueError("Unexpected end of input")
    ...
```

-   Here, exceptions are used to handle errors.

### Go

Go uses errors for error handling.

**Example:**

```go
func parseTokens(tokens []string) (SExp, []string, error) {
	if len(tokens) == 0 {
		return nil, tokens, errors.New("Unexpected end of input")
	}
    ...
}
```

-   Here, errors are used to handle errors.

### Haskell

Haskell uses `Either` for error handling.

**Example:**

```haskell
parseTokens' :: [String] -> (SExp, [String])
parseTokens' tokens = case tokens of
  [] -> error "Unmatched ("
  token:rest -> case token of
    "(" -> let (sexp, rest) = parseList rest in (SList sexp, rest)
    ")" -> error "Unmatched )"
    _ -> (Atom token, rest)
```

-   Here, `error` is used to handle errors.

### OCaml

OCaml uses exceptions for error handling.

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

-   Here, exceptions are used to handle errors.

### Lisp

Lisp uses `error` for error handling.

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

-   Here, `error` is used to handle errors.

### Clojure

Clojure uses exceptions for error handling.

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

-   Here, exceptions are used to handle errors.

### F#

F# uses exceptions for error handling.

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
```

-   Here, exceptions are used to handle errors.
</content>
</replace_in_file>
```
# Error Handling (Rust)

### What is Error Handling (Rust)?

Error handling in Rust uses the `Result` type to represent either a successful value or an error.

### Rust

**Example:**

```rust
fn parse_tokens(tokens: &mut Vec<String>) -> Result<SExp, String> {
    if tokens.is_empty() {
        return Err("Unexpected end of input".to_string());
    }
    ...
}
```

-   Here, `Result<SExp, String>` is used to represent either a successful `SExp` or an error message.
