# Interfaces and Protocols

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
-   In the `parser.swift` file, the `SExp` protocol defines the `toString` method, which any type conforming to this protocol must implement.

### Go

**Example:**

```go
type SExp interface {
	String() string
}
```

-   `SExp` is an interface that defines the `String` method, which any type implementing this interface must provide.
-   In the `parser.go` file, the `SExp` interface defines the `String` method, which any type implementing this interface must provide.

### C++

C++ uses abstract classes to define interfaces.

**Example:**

```cpp
class SExp {
public:
    virtual ~SExp() = default;
    virtual std::string toString() = 0;
};
```

-   `SExp` is an abstract class that defines the `toString` method, which any class inheriting from `SExp` must implement.

### C#

C# uses interfaces to define contracts.

**Example:**

```csharp
public interface SExp
{
    string ToString();
}
```

-   `SExp` is an interface that defines the `ToString` method, which any class implementing this interface must provide.

### Dart

Dart uses abstract classes to define interfaces.

**Example:**

```dart
abstract class SExp {
  String toString();
}
```

-   `SExp` is an abstract class that defines the `toString` method, which any class extending this class must implement.

### Kotlin

Kotlin uses interfaces to define contracts.

**Example:**

```kotlin
interface SExp {
    fun toString(): String
}
```

-   `SExp` is an interface that defines the `toString` method, which any class implementing this interface must provide.

### Python

Python uses abstract base classes to define interfaces.

**Example:**

```python
from abc import ABC, abstractmethod

class SExp(ABC):
    @abstractmethod
    def __str__(self):
        pass
```

-   `SExp` is an abstract base class that defines the `__str__` method, which any class inheriting from `SExp` must implement.

### Haskell

Haskell uses type classes to define interfaces.

**Example:**

```haskell
class SExp a where
    toString :: a -> String
```

-   `SExp` is a type class that defines the `toString` method, which any type implementing this type class must provide.

### OCaml

OCaml uses module signatures to define interfaces.

**Example:**

```ocaml
module type SExp = sig
  type t
  val to_string : t -> string
end
```

-   `SExp` is a module signature that defines the `to_string` method, which any module implementing this signature must provide.

### Rust

Rust uses traits to define interfaces.

**Example:**

```rust
trait SExp {
    fn to_string(&self) -> String;
}
```

-   `SExp` is a trait that defines the `to_string` method, which any type implementing this trait must provide.

### Lisp

Lisp does not have explicit interfaces, but you can use protocols.

**Example:**

```lisp
(defgeneric to-string (sexp)
  (:documentation "Convert an S-expression to a string."))
```

-   `to-string` is a generic function that acts as a protocol.

### Clojure

Clojure does not have explicit interfaces, but you can use protocols.

**Example:**

```clojure
(defprotocol SExp
  (to-string [this] "Convert an S-expression to a string."))
```

-   `SExp` is a protocol that defines the `to-string` method.

### F#

F# uses abstract classes to define interfaces.

**Example:**

```fsharp
type SExp = abstract
    abstract member ToString : unit -> string
```

-   `SExp` is an abstract type that defines the `ToString` method, which any type implementing this interface must provide.
</content>
</replace_in_file>
```
# Interfaces and Protocols

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
-   In the `parser.swift` file, the `SExp` protocol defines the `toString` method, which any type conforming to this protocol must implement.

### Go

**Example:**

```go
type SExp interface {
	String() string
}
```

-   `SExp` is an interface that defines the `String` method, which any type implementing this interface must provide.
-   In the `parser.go` file, the `SExp` interface defines the `String` method, which any type implementing this interface must provide.
