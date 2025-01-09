# Annotations

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

### C++

C++ does not have direct support for annotations, but you can use attributes.

**Example:**

```cpp
class SExp {
public:
    [[deprecated("Use a different method")]]
    virtual std::string toString() {
        return "SExp";
    }
};
```

-   `[[deprecated]]` is an attribute that provides additional information about the `toString` method.

### C#

C# uses attributes for annotations.

**Example:**

```csharp
public class SExp
{
    [Obsolete("Use a different method")]
    public virtual string ToString()
    {
        return "SExp";
    }
}
```

-   `[Obsolete]` is an attribute that provides additional information about the `ToString` method.

### Kotlin

Kotlin uses annotations.

**Example:**

```kotlin
class SExp {
    @Deprecated("Use a different method")
    open fun toString(): String {
        return "SExp"
    }
}
```

-   `@Deprecated` is an annotation that provides additional information about the `toString` method.

### Python

Python uses decorators for annotations.

**Example:**

```python
from abc import ABC, abstractmethod
import warnings

class SExp(ABC):
    @abstractmethod
    def __str__(self):
        return "SExp"

    @property
    def value(self):
        warnings.warn("Use a different method", DeprecationWarning)
        return None
```

-   `@abstractmethod` and `@property` are decorators that provide additional information about the methods.

### Go

Go does not have direct support for annotations, but you can use comments.

**Example:**

```go
// Deprecated: Use a different method
type SExp interface {
	String() string
}
```

-   `// Deprecated:` is a comment that provides additional information about the `SExp` interface.

### Haskell

Haskell does not have direct support for annotations, but you can use pragmas.

**Example:**

```haskell
{-# DEPRECATED stringOfSExp "Use a different method" #-}
stringOfSExp :: SExp -> String
stringOfSExp (Atom v) = v
stringOfSExp (SList l) = "(" <> concatMap show l <> ")"
```

-   `{-# DEPRECATED ... #-}` is a pragma that provides additional information about the `stringOfSExp` function.

### OCaml

OCaml does not have direct support for annotations, but you can use attributes.

**Example:**

```ocaml
[@@@deprecated "Use a different method"]
let string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"
```

-   `[@@@deprecated ... ]` is an attribute that provides additional information about the `string_of_sexp` function.

### Rust

Rust uses attributes for annotations.

**Example:**

```rust
#[derive(Debug, PartialEq)]
enum SExp {
    Atom(String),
    SList(Vec<SExp>),
}

impl SExp {
    #[deprecated(note = "Use a different method")]
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

-   `#[deprecated(...)]` is an attribute that provides additional information about the `to_string` method.

### Lisp

Lisp does not have direct support for annotations, but you can use documentation strings.

**Example:**

```lisp
(defstruct sexp
  (type nil)
  (value nil))

(defmethod to-string ((sexp sexp))
  "Convert an S-expression to a string. Use a different method."
  (if (eq (sexp-type sexp) 'atom)
      (sexp-value sexp)
      (format nil "(~{~a~})" (mapcar #'to-string (sexp-value sexp)))))
```

-   The documentation string provides additional information about the `to-string` method.

### Clojure

Clojure does not have direct support for annotations, but you can use metadata.

**Example:**

```clojure
(defn to-string
  [^:deprecated "Use a different method" sexp]
  (if (instance? Atom sexp)
      (:value sexp)
      (str sexp)))
```

-   `^:deprecated` is metadata that provides additional information about the `to-string` function.

### F#

F# uses attributes for annotations.

**Example:**

```fsharp
type SExp =
    [<Obsolete("Use a different method")>]
    abstract member ToString : unit -> string
```

-   `[<Obsolete(...)>]` is an attribute that provides additional information about the `ToString` method.
</content>
</replace_in_file>
```
# Annotations

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
