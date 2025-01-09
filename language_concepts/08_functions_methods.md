# Functions and Methods

### What are Functions?

Functions are reusable blocks of code that perform specific tasks.

### TypeScript

**Example:**

```typescript
function tokenize(input: string): string[] {
    return input
        .replace('(', ' ( ')
        .replace(')', ' ) ')
        .split(' ')
        .filter((token) => token.length > 0);
}
```

### Scala

**Example:**

```scala
def tokenize(input: String): List[String] = {
    input.replace("(", " ( ").replace(")", " ) ").split(" ").filter(_.nonEmpty).toList
}
```

### Swift

**Example:**

```swift
func tokenize(input: String) -> [String] {
    return input.replacingOccurrences(of: "(", with: " ( ").replacingOccurrences(of: ")", with: " ) ").components(separatedBy: " ").filter { !$0.isEmpty }
}
```

### Python

**Example:**

```python
def tokenize(input_string):
    input_string = input_string.replace("(", " ( ").replace(")", " ) ")
    return [token for token in input_string.split() if token]
```

### What are Methods?

Methods are functions that are associated with objects or classes.

### TypeScript

**Example:**

```typescript
function createAtom(value: string): SExp {
    return {
        type: 'atom',
        value: value,
        toString: () => value,
    };
}
```

-   `toString` is a method of the object returned by `createAtom`.

### Scala

**Example:**

```scala
case class Atom(value: String) extends SExp {
  override def toString: String = value
}
```

-   `toString` is a method of the `Atom` class.

### Swift

**Example:**

```swift
class Atom: SExp {
    let value: String
    init(value: String) {
        self.value = value
    }
    func toString() -> String {
        return value
    }
}
```

-   `toString` is a method of the `Atom` class.

### Python

**Example:**

```python
class Atom(SExp):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)
```

-   `__str__` is a method of the `Atom` class.

### C++

**Example (Function):**

```cpp
std::vector<std::string> tokenize(std::string input) {
    std::vector<std::string> tokens;
    std::string current_token;
    for (char c : input) {
        if (c == '(' || c == ')') {
            if (!current_token.empty()) {
                tokens.push_back(current_token);
                current_token = "";
            }
            tokens.push_back(std::string(1, c));
        } else if (c == ' ') {
            if (!current_token.empty()) {
                tokens.push_back(current_token);
                current_token = "";
            }
        } else {
            current_token += c;
        }
    }
    if (!current_token.empty()) {
        tokens.push_back(current_token);
    }
    return tokens;
}
```

**Example (Method):**

```cpp
class Atom : public SExp {
public:
    std::string value;
    Atom(std::string value) : value(value) {}
    std::string toString() override {
        return value;
    }
};
```

-   `toString` is a method of the `Atom` class.

### C#

**Example (Function):**

```csharp
public static List<string> Tokenize(string input)
{
    return input.Replace("(", " ( ").Replace(")", " ) ").Split(" ", StringSplitOptions.RemoveEmptyEntries).ToList();
}
```

**Example (Method):**

```csharp
public class Atom : SExp
{
    public string Value { get; }

    public Atom(string value)
    {
        Value = value;
    }

    public override string ToString()
    {
        return Value;
    }
}
```

-   `ToString` is a method of the `Atom` class.

### Dart

**Example (Function):**

```dart
List<String> tokenize(String input) {
  return input.replaceAll("(", " ( ").replaceAll(")", " ) ").split(" ").where((token) => token.isNotEmpty).toList();
}
```

**Example (Method):**

```dart
class Atom extends SExp {
  final String value;
  Atom(this.value);

  @override
  String toString() {
    return value;
  }
}
```

-   `toString` is a method of the `Atom` class.

### Kotlin

**Example (Function):**

```kotlin
fun tokenize(input: String): List<String> {
    return input.replace("(", " ( ").replace(")", " ) ").split(" ").filter { it.isNotEmpty() }
}
```

**Example (Method):**

```kotlin
class Atom(val value: String) : SExp() {
    override fun toString(): String {
        return value
    }
}
```

-   `toString` is a method of the `Atom` class.

### Go

**Example (Function):**

```go
func tokenize(input string) []string {
	input = strings.ReplaceAll(input, "(", " ( ")
	input = strings.ReplaceAll(input, ")", " ) ")
	return strings.Fields(input)
}
```

**Example (Method):**

```go
type Atom struct {
	value string
}

func (a Atom) String() string {
	return a.value
}
```

-   `String` is a method of the `Atom` struct.

### Haskell

**Example (Function):**

```haskell
tokenize :: String -> [String]
tokenize input = filter (not . null) $ words $ replace "(" " ( " $ replace ")" " ) " input
  where
    replace old new = unwords . map (\x -> if x == old then new else x) . words
```

**Example (Method):**

```haskell
data SExp = Atom String | SList [SExp]

instance Show SExp where
    show (Atom v) = v
    show (SList l) = "(" <> concatMap show l <> ")"
```

-   `show` is a method of the `SExp` data type.

### OCaml

**Example (Function):**

```ocaml
let tokenize input =
  let input = Str.global_replace (Str.regexp "(") " ( " input in
  let input = Str.global_replace (Str.regexp ")") " ) " input in
  List.filter (fun s -> s <> "") (String.split_on_char ' ' input)
```

**Example (Method):**

```ocaml
type sexp =
  | Atom of string
  | SList of sexp list

let rec string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"
```

-   `string_of_sexp` is a method of the `sexp` type.

### Rust

**Example (Function):**

```rust
fn tokenize(input: &str) -> Vec<String> {
    input
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split(" ")
        .filter(|token| !token.is_empty())
        .map(|token| token.to_string())
        .collect()
}
```

**Example (Method):**

```rust
#[derive(Debug, PartialEq)]
enum SExp {
    Atom(String),
    SList(Vec<SExp>),
}

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

-   `to_string` is a method of the `SExp` enum.

### Lisp

**Example (Function):**

```lisp
(defun tokenize (input)
  (let ((input (regex-replace-all "(" input " ( "))
        (input (regex-replace-all ")" input " ) ")))
    (remove "" (split-sequence " " input) :test #'string=)))
```

**Example (Method):**

```lisp
(defstruct sexp
  (type nil)
  (value nil))

(defmethod to-string ((sexp sexp))
  (if (eq (sexp-type sexp) 'atom)
      (sexp-value sexp)
      (format nil "(~{~a~})" (mapcar #'to-string (sexp-value sexp)))))
```

-   `to-string` is a method of the `sexp` structure.

### Clojure

**Example (Function):**

```clojure
(defn tokenize [input]
  (-> input
      (clojure.string/replace "(" " ( ")
      (clojure.string/replace ")" " ) ")
      (clojure.string/split #" ")
      (remove clojure.string/blank?)))
```

**Example (Method):**

```clojure
(defrecord Atom [value])

(defn to-string [sexp]
  (if (instance? Atom sexp)
      (:value sexp)
      (str sexp)))
```

-   `to-string` is a method of the `Atom` record.

### F#

**Example (Function):**

```fsharp
let tokenize input =
    input.Replace("(", " ( ").Replace(")", " ) ").Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
```

**Example (Method):**

```fsharp
type SExp =
    | Atom of string
    | SList of SExp list

let rec toString sexp =
    match sexp with
    | Atom v -> v
    | SList l -> "(" + String.concat "" (List.map toString l) + ")"
```

-   `toString` is a method of the `SExp` type.
</content>
</replace_in_file>
```
# Functions and Methods

### What are Functions?

Functions are reusable blocks of code that perform specific tasks.

### TypeScript

**Example:**

```typescript
function tokenize(input: string): string[] {
    return input
        .replace('(', ' ( ')
        .replace(')', ' ) ')
        .split(' ')
        .filter((token) => token.length > 0);
}
```

### Scala

**Example:**

```scala
def tokenize(input: String): List[String] = {
    input.replace("(", " ( ").replace(")", " ) ").split(" ").filter(_.nonEmpty).toList
}
```

### Swift

**Example:**

```swift
func tokenize(input: String) -> [String] {
    return input.replacingOccurrences(of: "(", with: " ( ").replacingOccurrences(of: ")", with: " ) ").components(separatedBy: " ").filter { !$0.isEmpty }
}
```

### Python

**Example:**

```python
def tokenize(input_string):
    input_string = input_string.replace("(", " ( ").replace(")", " ) ")
    return [token for token in input_string.split() if token]
```

### What are Methods?

Methods are functions that are associated with objects or classes.

### TypeScript

**Example:**

```typescript
function createAtom(value: string): SExp {
    return {
        type: 'atom',
        value: value,
        toString: () => value,
    };
}
```

-   `toString` is a method of the object returned by `createAtom`.

### Scala

**Example:**

```scala
case class Atom(value: String) extends SExp {
  override def toString: String = value
}
```

-   `toString` is a method of the `Atom` class.

### Swift

**Example:**

```swift
class Atom: SExp {
    let value: String
    init(value: String) {
        self.value = value
    }
    func toString() -> String {
        return value
    }
}
```

-   `toString` is a method of the `Atom` class.

### Python

**Example:**

```python
class Atom(SExp):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)
```

-   `__str__` is a method of the `Atom` class.
