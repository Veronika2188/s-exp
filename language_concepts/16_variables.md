# Variables

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

### C++

**Example:**

```cpp
std::string input = "(+ 1 (* 2 3))";
std::vector<std::string> tokens = tokenize(input);
```

### C#

**Example:**

```csharp
string inputString = "(+ 1 (* 2 3))";
List<string> tokens = Tokenize(inputString);
```

### Dart

**Example:**

```dart
String input = "(+ 1 (* 2 3))";
List<String> tokens = tokenize(input);
```

### Kotlin

**Example:**

```kotlin
var input = "(+ 1 (* 2 3))"
val tokens = tokenize(input)
```

### Go

**Example:**

```go
input := "(+ 1 (* 2 3))"
tokens := tokenize(input)
```

### Haskell

**Example:**

```haskell
let input = "(+ 1 (* 2 3))"
let tokens = tokenize input
```

### OCaml

**Example:**

```ocaml
let input = "(+ 1 (* 2 3))"
let tokens = tokenize input
```

### Rust

**Example:**

```rust
let input = "(+ 1 (* 2 3))";
let mut tokens = tokenize(input);
```

### Lisp

**Example:**

```lisp
(let ((input "(+ 1 (* 2 3))"))
  (let ((tokens (tokenize input)))
    (print tokens)))
```

### Clojure

**Example:**

```clojure
(def input "(+ 1 (* 2 3))")
(def tokens (tokenize input))
```

### F#

**Example:**

```fsharp
let input = "(+ 1 (* 2 3))"
let tokens = tokenize input
