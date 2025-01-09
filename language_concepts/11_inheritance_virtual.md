# Inheritance and Virtual Functions

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

### C#

**Example:**

```csharp
public abstract class SExp
{
    public virtual string ToString()
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
```

-   `SExp` is an abstract class with a virtual `ToString` method, and `Atom` overrides this method.

### Dart

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
```

-   `SExp` is an abstract class, and `Atom` overrides the `toString` method.

### Kotlin

**Example:**

```kotlin
open class SExp {
    open fun toString(): String {
        return "SExp"
    }
}

class Atom(val value: String) : SExp() {
    override fun toString(): String {
        return value
    }
}
```

-   `SExp` is an open class with an open `toString` method, and `Atom` overrides this method.

### Python

**Example:**

```python
from abc import ABC, abstractmethod

class SExp(ABC):
    @abstractmethod
    def __str__(self):
        return "SExp"

class Atom(SExp):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)
```

-   `SExp` is an abstract base class with an abstract `__str__` method, and `Atom` overrides this method.

### Go

Go does not have direct support for inheritance or virtual functions, but you can use interfaces and embedding.

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
```

-   `SExp` is an interface, and `Atom` implements the `String` method.

### Haskell

Haskell does not have direct support for inheritance or virtual functions, but you can use type classes.

**Example:**

```haskell
data SExp = Atom String | SList [SExp]

instance Show SExp where
    show (Atom v) = v
    show (SList l) = "(" <> concatMap show l <> ")"
```

-   `SExp` is a data type, and the `Show` instance provides a method for converting it to a string.

### OCaml

OCaml does not have direct support for inheritance or virtual functions, but you can use modules and records.

**Example:**

```ocaml
type sexp =
  | Atom of string
  | SList of sexp list

let rec string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"
```

-   `sexp` is a type, and `string_of_sexp` is a function that handles different cases of the type.

### Rust

Rust uses traits and polymorphism to achieve similar results.

**Example:**

```rust
trait SExp {
    fn to_string(&self) -> String;
}

#[derive(Debug, PartialEq)]
enum SExpEnum {
    Atom(String),
    SList(Vec<SExpEnum>),
}

impl SExp for SExpEnum {
    fn to_string(&self) -> String {
        match self {
            SExpEnum::Atom(v) => v.clone(),
            SExpEnum::SList(l) => {
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

-   `SExp` is a trait, and `SExpEnum` implements the `to_string` method.

### Lisp

Lisp uses generic functions to achieve polymorphism.

**Example:**

```lisp
(defgeneric to-string (sexp)
  (:documentation "Convert an S-expression to a string."))

(defstruct sexp
  (type nil)
  (value nil))

(defmethod to-string ((sexp sexp))
  (if (eq (sexp-type sexp) 'atom)
      (sexp-value sexp)
      (format nil "(~{~a~})" (mapcar #'to-string (sexp-value sexp)))))
```

-   `to-string` is a generic function that can be specialized for different types.

### Clojure

Clojure uses protocols to achieve polymorphism.

**Example:**

```clojure
(defprotocol SExp
  (to-string [this] "Convert an S-expression to a string."))

(defrecord Atom [value])

(defn to-string [sexp]
  (if (instance? Atom sexp)
      (:value sexp)
      (str sexp)))
```

-   `SExp` is a protocol that defines the `to-string` method, and `Atom` implements this protocol.

### F#

F# uses abstract members and inheritance to achieve polymorphism.

**Example:**

```fsharp
type SExp = abstract
    abstract member ToString : unit -> string

type Atom =
    { Value: string }
    interface SExp with
        member this.ToString() = this.Value

type SList =
    { Children: SExp list }
    interface SExp with
        member this.ToString() = "(" + String.concat "" (List.map (fun x -> x.ToString()) this.Children) + ")"
```

-   `SExp` is an abstract type with an abstract `ToString` method, and `Atom` and `SList` implement this interface.
</content>
</replace_in_file>
```
# Inheritance and Virtual Functions

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
