# Enums, Algebraic Data Types (ADTs), and Sealed Classes

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
-   In the `parser.swift` file, the `ParserError` enum defines the possible errors that can occur during parsing.

### Rust

**Example:**

```rust
#[derive(Debug, PartialEq)]
enum SExp {
    Atom(String),
    SList(Vec<SExp>),
}
```

-   `SExp` is an enum that defines the structure of an S-expression.
-   In the `parser.rs` file, the `SExp` enum defines the structure of an S-expression, which can be either an `Atom` or an `SList`.

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
-   In the `Parser.scala` file, the `SExp` trait is a sealed trait, and `Atom` and `SList` are case classes that extend `SExp`, forming an ADT.

### C++

C++ does not have direct support for enums with associated data, but you can use a class hierarchy.

**Example:**

```cpp
class SExp {
public:
    virtual ~SExp() = default;
    virtual std::string toString() = 0;
};

class Atom : public SExp {
public:
    std::string value;
    Atom(std::string value) : value(value) {}
    std::string toString() override {
        return value;
    }
};

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

-   `SExp` is an abstract class, and `Atom` and `SList` are concrete classes that extend `SExp`. This defines an ADT where an `SExp` can be either an `Atom` or an `SList`.

### C#

C# uses abstract classes and inheritance to define ADTs.

**Example:**

```csharp
public abstract class SExp
{
    public override string ToString()
    {
        return "SExp";
    }
}

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

-   `SExp` is an abstract class, and `Atom` and `SList` are concrete classes that extend `SExp`. This defines an ADT where an `SExp` can be either an `Atom` or an `SList`.

### Dart

Dart uses abstract classes and inheritance to define ADTs.

**Example:**

```dart
abstract class SExp {
  String toString();
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
    return "(" + children.map((child) => child.toString()).join() + ")";
  }
}
```

-   `SExp` is an abstract class, and `Atom` and `SList` are concrete classes that extend `SExp`. This defines an ADT where an `SExp` can be either an `Atom` or an `SList`.

### Python

Python uses classes and inheritance to define ADTs.

**Example:**

```python
from abc import ABC, abstractmethod

class SExp(ABC):
    @abstractmethod
    def __str__(self):
        pass

class Atom(SExp):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

class SList(SExp):
    def __init__(self, children):
        self.children = children

    def __str__(self):
        return "(" + " ".join(map(str, self.children)) + ")"
```

-   `SExp` is an abstract base class, and `Atom` and `SList` are concrete classes that inherit from `SExp`. This defines an ADT where an `SExp` can be either an `Atom` or an `SList`.

### Go

Go uses interfaces and structs to define ADTs.

**Example:**

```go
type SExp interface {
	String() string
}

type Atom struct {
	value string
}

func (a Atom) String() string {
	return a.value
}

type SList struct {
	children []SExp
}

func (l SList) String() string {
	result := "("
	for _, child := range l.children {
		result += child.String()
	}
	result += ")"
	return result
}
```

-   `SExp` is an interface, and `Atom` and `SList` are structs that implement the `SExp` interface. This defines an ADT where an `SExp` can be either an `Atom` or an `SList`.

### Lisp

Lisp uses structures to define ADTs.

**Example:**

```lisp
(defstruct sexp
  (type nil)
  (value nil))

(defun make-atom (value)
  (make-sexp :type 'atom :value value))

(defun make-list (children)
  (make-sexp :type 'list :value children))

(defmethod to-string ((sexp sexp))
  (if (eq (sexp-type sexp) 'atom)
      (sexp-value sexp)
      (format nil "(~{~a~})" (mapcar #'to-string (sexp-value sexp)))))
```

-   `sexp` is a structure that has fields `type` and `value`.

### Clojure

Clojure uses records to define ADTs.

**Example:**

```clojure
(defrecord Atom [value])
(defrecord SList [children])

(defn make-atom [value]
  (->Atom value))

(defn make-list [children]
  (->SList children))

(defn to-string [sexp]
  (cond
    (instance? Atom sexp) (:value sexp)
    (instance? SList sexp) (str "(" (clojure.string/join " " (map to-string (:children sexp))) ")")
    :else (str sexp)))
```

-   `Atom` and `SList` are records that represent the different cases of the ADT.
</content>
</replace_in_file>
```
# Enums, Algebraic Data Types (ADTs), and Sealed Classes

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
-   In the `parser.swift` file, the `ParserError` enum defines the possible errors that can occur during parsing.

### Rust

**Example:**

```rust
#[derive(Debug, PartialEq)]
enum SExp {
    Atom(String),
    SList(Vec<SExp>),
}
```

-   `SExp` is an enum that defines the structure of an S-expression.
-   In the `parser.rs` file, the `SExp` enum defines the structure of an S-expression, which can be either an `Atom` or an `SList`.

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
-   In the `Parser.scala` file, the `SExp` trait is a sealed trait, and `Atom` and `SList` are case classes that extend `SExp`, forming an ADT.

### F#

**Example:**

```fsharp
type SExp =
    | Atom of string
    | SList of SExp list
```

-   `SExp` is a discriminated union that can be either an `Atom` (containing a string) or an `SList` (containing a list of `SExp` values).
-   In the `parser.fs` file, the `SExp` type is a discriminated union that can be either an `Atom` or an `SList`.

### Haskell

**Example:**

```haskell
data SExp = Atom String | SList [SExp]
```

-   `SExp` is an ADT that can be either an `Atom` (containing a `String`) or an `SList` (containing a list of `SExp` values).
-   In the `parser.hs` file, the `SExp` data type is an ADT that can be either an `Atom` or an `SList`.

### OCaml

**Example:**

```ocaml
type sexp =
  | Atom of string
  | SList of sexp list
```

-   `sexp` is an ADT that can be either an `Atom` (containing a `string`) or an `SList` (containing a list of `sexp` values).
-   In the `parser.ml` file, the `sexp` type is an ADT that can be either an `Atom` or an `SList`.

### Rust

**Example:**

```rust
#[derive(Debug, PartialEq)]
enum SExp {
    Atom(String),
    SList(Vec<SExp>),
}
```

-   `SExp` is an ADT that can be either an `Atom` (containing a `String`) or an `SList` (containing a `Vec<SExp>`).
-   In the `parser.rs` file, the `SExp` enum is an ADT that can be either an `Atom` or an `SList`.

### What are Sealed Classes?

Sealed classes (or traits) restrict the possible subclasses or implementations to those defined in the same file.

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

### Scala

**Example:**

```scala
sealed trait SExp {
  def toString: String
}
```

### Comparison of Enums, ADTs, and Sealed Classes

-   **Enums:** Define a type with a finite set of named values. They are often used to represent a set of options or states.
-   **ADTs:** Combine different types to create more complex types. They are often used to represent data structures. ADTs can be implemented using enums, sealed classes, or discriminated unions.
-   **Sealed Classes:** Restrict the possible subclasses or implementations to those defined in the same file. They are often used to represent a set of related types.

ADTs and sealed classes are similar in that they both define a set of related types. However, ADTs are more general and can be used to represent a wider range of data structures. Sealed classes are more specific and are often used to represent a set of related types that are known at compile time.
