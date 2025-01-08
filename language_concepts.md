# Language Concepts in S-Expression Parsers

This document explores various programming language concepts used in the implementations of an S-expression parser. We'll examine how these concepts are applied in different languages like TypeScript, Scala, Swift, and Python, using examples from the project code.

## 1. Types

### What are Types?

In programming, types classify values and determine what operations can be performed on them. They help ensure code correctness and prevent errors.

### TypeScript

TypeScript is a statically-typed language, meaning types are checked at compile time. It uses explicit type annotations.

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

-   Here, `value` is explicitly typed as `string`, and the function `createAtom` is declared to return a value of type `SExp`.

### Scala

Scala is also statically-typed and uses both explicit type annotations and type inference.

**Example:**

```scala
case class Atom(value: String) extends SExp {
  override def toString: String = value
}
```

-   Here, `value` is explicitly typed as `String`. Scala can also infer types in many cases, reducing the need for explicit annotations.

### Swift

Swift is a statically-typed language that uses type inference and explicit type annotations.

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

-   Here, `value` is explicitly typed as `String`. Swift also uses type inference to determine types when they are not explicitly declared.

### Python

Python is a dynamically-typed language, meaning types are checked at runtime. Variables do not have explicit type declarations.

**Example:**

```python
class Atom(SExp):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)
```

-   Here, `value` is not explicitly typed. Python determines the type of `value` at runtime based on the assigned value.

## 2. Interfaces and Protocols

### What are Interfaces and Protocols?

Interfaces (in TypeScript) and protocols (in Swift) define contracts that classes or objects must adhere to. They specify methods and properties that implementing types must provide.

### TypeScript

**Example:**

```typescript
interface SExp {
    type: string;
    value: any;
    toString(): string;
}
```

-   `SExp` is an interface that defines the structure of an S-expression, including a `type`, a `value`, and a `toString` method.

### Swift

**Example:**

```swift
protocol SExp {
    func toString() -> String
}
```

-   `SExp` is a protocol that defines the `toString` method, which any type conforming to this protocol must implement.

## 3. Classes

### What are Classes?

Classes are blueprints for creating objects. They encapsulate data (properties) and behavior (methods).

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

-   While TypeScript uses classes, this example shows a function that returns an object that conforms to the `SExp` interface.

### Scala

**Example:**

```scala
case class Atom(value: String) extends SExp {
  override def toString: String = value
}
```

-   `Atom` is a case class, a special type of class in Scala that is often used to represent data. It extends the `SExp` trait.

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

-   `Atom` is a class that conforms to the `SExp` protocol.

### Python

**Example:**

```python
class Atom(SExp):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)
```

-   `Atom` is a class that inherits from `SExp`.

## 4. Enums

### What are Enums?

Enums (enumerations) define a type with a finite set of named values.

### Swift

**Example:**

```swift
enum ParserError: Error {
    case unexpectedEndOfInput
    case unmatchedClosingParenthesis
    case unmatchedOpeningParenthesis
    case invalidSExpression
}
```

-   `ParserError` is an enum that defines the possible errors that can occur during parsing.

## 5. ADT (Algebraic Data Types)

### What are ADTs?

ADTs are a way of combining different types to create more complex types. They are often used to represent data structures.

### Scala

**Example:**

```scala
sealed trait SExp {
  def toString: String
}

case class Atom(value: String) extends SExp {
  override def toString: String = value
}

case class SList(children: List[SExp]) extends SExp {
  override def toString: String = "(" + children.map(_.toString).mkString + ")"
}
```

-   `SExp` is a sealed trait, and `Atom` and `SList` are case classes that extend `SExp`. This defines an ADT where an `SExp` can be either an `Atom` or an `SList`.

## 6. Type Classes

### What are Type Classes?

Type classes provide a way to add new behavior to existing types without modifying the types themselves.

### Scala

**Example:**

```scala
sealed trait SExp {
  def toString: String
}
```

-   The `SExp` trait can be considered a type class, as it defines the `toString` method that can be implemented by different types.

## 7. Traits

### What are Traits?

Traits are similar to interfaces but can also include method implementations.

### Scala

**Example:**

```scala
sealed trait SExp {
  def toString: String
}
```

-   `SExp` is a trait that defines the `toString` method.

## 8. Structures

### What are Structures?

Structures are user-defined data types that group together variables of different types.

### C

**Example:**

```c
typedef struct SExp {
    char* type;
    void* value;
} SExp;
```

-   This C code shows how a struct is used to define an S-expression.

## 9. Pointers

### What are Pointers?

Pointers are variables that store memory addresses. They are used to access and manipulate data in memory directly.

### C

**Example:**

```c
SExp* create_atom(char* value) {
    SExp* atom = (SExp*)malloc(sizeof(SExp));
    atom->type = "atom";
    atom->value = value;
    return atom;
}
```

-   Here, `SExp*` is a pointer to an `SExp` struct.

## 10. Dynamic Memory Allocation

### What is Dynamic Memory Allocation?

Dynamic memory allocation is the process of allocating memory during runtime.

### C

**Example:**

```c
SExp* create_atom(char* value) {
    SExp* atom = (SExp*)malloc(sizeof(SExp));
    atom->type = "atom";
    atom->value = value;
    return atom;
}
```

-   Here, `malloc` is used to allocate memory for the `SExp` struct.

## 11. Manual Memory Management

### What is Manual Memory Management?

Manual memory management is the process of explicitly allocating and deallocating memory.

### C

**Example:**

```c
char* sexp_to_string(SExp* sexp) {
    if (strcmp(sexp->type, "atom") == 0) {
        return (char*)sexp->value;
    } else {
        SExp** children = (SExp**)sexp->value;
        char* result = (char*)malloc(sizeof(char) * 1024);
        strcpy(result, "(");
        int i = 0;
        while (children[i] != NULL) {
            strcat(result, sexp_to_string(children[i]));
            i++;
        }
        strcat(result, ")");
        return result;
    }
}
```

-   Here, `malloc` is used to allocate memory, and `free` is used to deallocate memory.

## 12. Functions

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

## 13. Maps

### What are Maps?

Maps (also known as dictionaries or associative arrays) are data structures that store key-value pairs.

### Clojure

**Example:**

```clojure
(defn make-atom [value]
  {:type :atom :value value})
```

-   Here, a map is used to represent an atom, with keys `:type` and `:value`.

## 14. Lists

### What are Lists?

Lists are ordered collections of items.

### Clojure

**Example:**

```clojure
(defn make-list [children]
  {:type :list :value children})
```

-   Here, a list is used to represent the children of an S-expression.

## 15. Recursion

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

## 16. Methods

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

## 17. Inheritance

### What is Inheritance?

Inheritance is a mechanism where a class inherits properties and methods from a parent class.

### C++

**Example:**

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

-   Here, `Atom` inherits from `SExp`.

## 18. Virtual Functions

### What are Virtual Functions?

Virtual functions are functions that can be overridden by derived classes.

### C++

**Example:**

```cpp
class SExp {
public:
    virtual ~SExp() = default;
    virtual std::string toString() {
        return "SExp";
    }
};
```

-   Here, `toString` is a virtual function that can be overridden by derived classes like `Atom` and `SList`.

## 19. Vectors

### What are Vectors?

Vectors are dynamic arrays that can grow or shrink in size.

### C++

**Example:**

```cpp
class SList : public SExp {
public:
    std::vector<SExp*> children;
    SList(std::vector<SExp*> children) : children(children) {}
    std::string toString() override {
        std::string result = "(";
        for (SExp* child : children) {
            result += child->toString();
        }
        result += ")";
        return result;
    }
    ~SList() {
        for (SExp* child : children) {
            delete child;
        }
    }
};
```

-   Here, `std::vector` is used to store the children of an `SList`.

## 20. Dynamic Memory Allocation (C++)

### What is Dynamic Memory Allocation (C++)?

Dynamic memory allocation in C++ uses `new` and `delete` to allocate and deallocate memory.

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

-   Here, `new` is used to allocate memory for `Atom` and `SList` objects.

## 21. Manual Memory Management (C++)

### What is Manual Memory Management (C++)?

Manual memory management in C++ requires explicit deallocation of memory using `delete`.

### C++

**Example:**

```cpp
int main() {
    std::string input = "(+ 1 (* 2 3))";
    SExp* result = parse(input);
    std::cout << result->toString() << std::endl;
    delete result;
    return 0;
}
```

-   Here, `delete` is used to deallocate the memory allocated for the `SExp` object.

## 22. Constants

### What are Constants?

Constants are values that cannot be changed after they are defined.

### TypeScript

**Example:**

```typescript
const input = '(+ 1 (* 2 3))';
```

### Scala

**Example:**

```scala
val input = "(+ 1 (* 2 3))"
```

### Swift

**Example:**

```swift
let parser = Parser()
```

### Python

**Example:**

```python
input_string = "(+ 1 (* 2 3))"
```

## 23. Abstract Classes

### What are Abstract Classes?

Abstract classes are classes that cannot be instantiated directly and are meant to be base classes for other classes.

### C#

**Example:**

```csharp
public abstract class SExp
{
    public override string ToString()
    {
        return "SExp";
    }
}
```

-   Here, `SExp` is an abstract class that cannot be instantiated directly.

## 24. Lists (C#)

### What are Lists (C#)?

Lists in C# are dynamic arrays that can grow or shrink in size.

### C#

**Example:**

```csharp
public class SList : SExp
{
    public List<SExp> Children { get; }

    public SList(List<SExp> children)
    {
        Children = children;
    }

    public override string ToString()
    {
        return "(" + string.Join("", Children.Select(child => child.ToString())) + ")";
    }
}
```

-   Here, `List<SExp>` is used to store the children of an `SList`.

## 25. LINQ

### What is LINQ?

LINQ (Language Integrated Query) is a feature in C# that allows you to query and manipulate data using a consistent syntax.

### C#

**Example:**

```csharp
public class SList : SExp
{
    public List<SExp> Children { get; }

    public SList(List<SExp> children)
    {
        Children = children;
    }

    public override string ToString()
    {
        return "(" + string.Join("", Children.Select(child => child.ToString())) + ")";
    }
}
```

-   Here, `Children.Select(child => child.ToString())` uses LINQ to transform the list of children into a list of strings.

## 26. Annotations

### What are Annotations?

Annotations are metadata that provide additional information about the code.

### Dart

**Example:**

```dart
class SExp {
  @override
  String toString() {
    return 'SExp';
  }
}
```

-   Here, `@override` is an annotation that indicates that the `toString` method is overriding a method from a parent class.

## 27. Discriminated Unions

### What are Discriminated Unions?

Discriminated unions (also known as tagged unions or sum types) are a way to define a type that can be one of several different cases.

### F#

**Example:**

```fsharp
type SExp =
    | Atom of string
    | SList of SExp list
```

-   Here, `SExp` is a discriminated union that can be either an `Atom` (containing a string) or an `SList` (containing a list of `SExp` values).

## 28. Interfaces (Go)

### What are Interfaces (Go)?

Interfaces in Go are a way to define a set of methods that a type must implement.

### Go

**Example:**

```go
type SExp interface {
	String() string
}
```

-   Here, `SExp` is an interface that defines the `String` method.

## 29. Structs (Go)

### What are Structs (Go)?

Structs in Go are user-defined types that group together variables of different types.

### Go

**Example:**

```go
type Atom struct {
	value string
}
```

-   Here, `Atom` is a struct that has a field `value` of type `string`.

## 30. Methods (Go)

### What are Methods (Go)?

Methods in Go are functions that are associated with a specific type.

### Go

**Example:**

```go
func (a Atom) String() string {
	return a.value
}
```

-   Here, `String` is a method of the `Atom` struct.

## 31. Algebraic Data Types (Haskell)

### What are Algebraic Data Types (Haskell)?

Algebraic data types (ADTs) in Haskell are a way to define complex types by combining other types.

### Haskell

**Example:**

```haskell
data SExp = Atom String | SList [SExp]
```

-   Here, `SExp` is an ADT that can be either an `Atom` (containing a `String`) or an `SList` (containing a list of `SExp` values).

## 32. Pattern Matching (Haskell)

### What is Pattern Matching (Haskell)?

Pattern matching in Haskell is a way to match values against patterns and execute different code based on the match.

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

## 33. Recursion (Haskell)

### What is Recursion (Haskell)?

Recursion in Haskell is a technique where a function calls itself to solve a problem.

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

## 34. Type Classes (Haskell)

### What are Type Classes (Haskell)?

Type classes in Haskell are a way to define a set of operations that can be performed on different types.

### Haskell

**Example:**

```haskell
instance Show SExp where
    show (Atom v) = v
    show (SList l) = "(" <> concatMap show l <> ")"
```

-   Here, `Show` is a type class that defines the `show` method, which is used to convert a value to a string.

## 35. Annotations (Java)

### What are Annotations (Java)?

Annotations in Java are metadata that provide additional information about the code.

### Java

**Example:**

```java
class SExp {
    @Override
    public String toString() {
        return "SExp";
    }
}
```

-   Here, `@Override` is an annotation that indicates that the `toString` method is overriding a method from a parent class.

## 36. Inheritance (JavaScript)

### What is Inheritance (JavaScript)?

Inheritance in JavaScript is a mechanism where a class inherits properties and methods from a parent class using the `extends` keyword.

### JavaScript

**Example:**

```javascript
class Atom extends SExp {
    constructor(value) {
        super();
        this.value = value;
    }

    toString() {
        return this.value;
    }
}
```

-   Here, `Atom` inherits from `SExp`.

## 37. Sealed Classes (Kotlin)

### What are Sealed Classes (Kotlin)?

Sealed classes in Kotlin are used to represent restricted class hierarchies, similar to ADTs.

### Kotlin

**Example:**

```kotlin
sealed class SExp {
    override fun toString(): String {
        return "SExp"
    }
}
```

-   Here, `SExp` is a sealed class that can only be extended by classes defined in the same file.

## 38. Structures (Lisp)

### What are Structures (Lisp)?

Structures in Lisp are user-defined data types that group together variables of different types.

### Common Lisp

**Example:**

```lisp
(defstruct sexp
  (type nil)
  (value nil))
```

-   Here, `sexp` is a structure that has fields `type` and `value`.

## 39. Dynamic Typing (Lisp)

### What is Dynamic Typing (Lisp)?

Dynamic typing in Lisp means that types are checked at runtime, and variables do not have explicit type declarations.

### Common Lisp

**Example:**

```lisp
(defun make-atom (value)
  (make-sexp :type 'atom :value value))
```

-   Here, `value` is not explicitly typed. Lisp determines the type of `value` at runtime based on the assigned value.

## 38. Structures (Lisp)

### What are Structures (Lisp)?

Structures in Lisp are user-defined data types that group together variables of different types.

### Common Lisp

**Example:**

```lisp
(defstruct sexp
  (type nil)
  (value nil))
```

-   Here, `sexp` is a structure that has fields `type` and `value`.

## 39. Dynamic Typing (Lisp)

### What is Dynamic Typing (Lisp)?

Dynamic typing in Lisp means that types are checked at runtime, and variables do not have explicit type declarations.

### Common Lisp

**Example:**

```lisp
(defun make-atom (value)
  (make-sexp :type 'atom :value value))
```

-   Here, `value` is not explicitly typed. Lisp determines the type of `value` at runtime based on the assigned value.

## 40. Algebraic Data Types (OCaml)

### What are Algebraic Data Types (OCaml)?

Algebraic data types (ADTs) in OCaml are a way to define complex types by combining other types.

### OCaml

**Example:**

```ocaml
type sexp =
  | Atom of string
  | SList of sexp list
```

-   Here, `sexp` is an ADT that can be either an `Atom` (containing a `string`) or an `SList` (containing a list of `sexp` values).

## 41. Pattern Matching (OCaml)

### What is Pattern Matching (OCaml)?

Pattern matching in OCaml is a way to match values against patterns and execute different code based on the match.

### OCaml

**Example:**

```ocaml
let rec string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"
```

-   Here, `string_of_sexp` uses pattern matching to handle different cases of the `sexp` type.

## 42. Recursion (OCaml)

### What is Recursion (OCaml)?

Recursion in OCaml is a technique where a function calls itself to solve a problem.

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

## 12. Variables

### What are Variables?

Variables are named storage locations that can hold values that can change during program execution.

### TypeScript

**Example:**

```typescript
let tokens = tokenize(input);
```

### Scala

**Example:**

```scala
var tokens = tokenize(input)
```

### Swift

**Example:**

```swift
var tokens = tokenize(input: input)
```

### Python

**Example:**

```python
tokens = tokenize(input_string)
```

## 13. Pattern Matching

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

## 14. Sealed Classes

### What are Sealed Classes?

Sealed classes (or traits) restrict the possible subclasses or implementations to those defined in the same file.

### Scala

**Example:**

```scala
sealed trait SExp {
  def toString: String
}
```

## 15. Comparison of Languages

### Similarities

-   All languages implement the core parsing logic using a recursive descent approach.
-   All languages use functions or methods to tokenize the input string.
-   All languages use some form of data structure to represent S-expressions (classes, case classes, structs, etc.).

### Differences

-   **Typing:** TypeScript, Scala, and Swift are statically-typed, while Python is dynamically-typed.
-   **Syntax:** Each language has its own syntax for defining classes, functions, and other language constructs.
-   **Features:** Scala and Swift use ADTs and pattern matching extensively, while TypeScript uses interfaces and classes. Python uses dynamic typing and classes.

## Conclusion

The S-expression parser implementations demonstrate various language concepts across different programming languages. While the core logic remains similar, each language uses its own syntax and features to achieve the same result. Understanding these concepts is crucial for writing effective code in each language. The choice of language depends on the specific requirements and preferences of the developer.
