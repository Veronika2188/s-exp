# Maps, Lists, and Vectors

### What are Maps?

Maps (also known as dictionaries or associative arrays) are data structures that store key-value pairs.

### Clojure

**Example:**

```clojure
(defn make-atom [value]
  {:type :atom :value value})
```

-   Here, a map is used to represent an atom, with keys `:type` and `:value`.

### What are Lists?

Lists are ordered collections of items.

### Clojure

**Example:**

```clojure
(defn make-list [children]
  {:type :list :value children})
```

-   Here, a list is used to represent the children of an S-expression.

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

-   Here, `Vec<SExp>` is used to store the children of an `SList`.

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

### C++ (Maps)

C++ uses `std::map` for maps.

**Example:**

```cpp
#include <map>
#include <string>
#include <iostream>

int main() {
    std::map<std::string, int> myMap;
    myMap["one"] = 1;
    myMap["two"] = 2;
    std::cout << myMap["one"] << std::endl;
    return 0;
}
```

### C++ (Lists)

C++ uses `std::list` for lists.

**Example:**

```cpp
#include <list>
#include <iostream>

int main() {
    std::list<int> myList = {1, 2, 3};
    for (int i : myList) {
        std::cout << i << std::endl;
    }
    return 0;
}
```

### C# (Maps)

C# uses `Dictionary` for maps.

**Example:**

```csharp
using System;
using System.Collections.Generic;

public class Example
{
    public static void Main(string[] args)
    {
        Dictionary<string, int> myMap = new Dictionary<string, int>();
        myMap["one"] = 1;
        myMap["two"] = 2;
        Console.WriteLine(myMap["one"]);
    }
}
```

### C# (Lists)

C# uses `List` for lists.

**Example:**

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

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

### Dart (Maps)

Dart uses `Map` for maps.

**Example:**

```dart
void main() {
  Map<String, int> myMap = {
    "one": 1,
    "two": 2,
  };
  print(myMap["one"]);
}
```

### Dart (Lists)

Dart uses `List` for lists.

**Example:**

```dart
class SList extends SExp {
  final List<SExp> children;
  SList(this.children);

  @override
  String toString() {
    return "(" + children.map((child) => child.toString()).join() + ")";
  }
}
```

### Kotlin (Maps)

Kotlin uses `Map` for maps.

**Example:**

```kotlin
fun main() {
    val myMap = mapOf("one" to 1, "two" to 2)
    println(myMap["one"])
}
```

### Kotlin (Lists)

Kotlin uses `List` for lists.

**Example:**

```kotlin
class SList(val children: List<SExp>) : SExp() {
    override fun toString(): String {
        return "(" + children.joinToString("") { it.toString() } + ")"
    }
}
```

### Python (Maps)

Python uses `dict` for maps.

**Example:**

```python
my_map = {"one": 1, "two": 2}
print(my_map["one"])
```

### Python (Lists)

Python uses `list` for lists.

**Example:**

```python
class SList(SExp):
    def __init__(self, children):
        self.children = children

    def __str__(self):
        return "(" + " ".join(map(str, self.children)) + ")"
```

### Go (Maps)

Go uses `map` for maps.

**Example:**

```go
func main() {
	myMap := map[string]int{
		"one": 1,
		"two": 2,
	}
	fmt.Println(myMap["one"])
}
```

### Go (Lists)

Go uses slices for lists.

**Example:**

```go
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

### Haskell (Lists)

Haskell uses lists for lists.

**Example:**

```haskell
data SExp = Atom String | SList [SExp]

instance Show SExp where
    show (Atom v) = v
    show (SList l) = "(" <> concatMap show l <> ")"
```

### OCaml (Lists)

OCaml uses lists for lists.

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

### Rust (Maps)

Rust uses `HashMap` for maps.

**Example:**

```rust
use std::collections::HashMap;

fn main() {
    let mut my_map = HashMap::new();
    my_map.insert("one", 1);
    my_map.insert("two", 2);
    println!("{}", my_map.get("one").unwrap());
}
```

### Rust (Vectors)

Rust uses `Vec` for vectors.

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

### Lisp (Maps)

Lisp uses hash tables for maps.

**Example:**

```lisp
(let ((my-map (make-hash-table)))
  (setf (gethash "one" my-map) 1)
  (setf (gethash "two" my-map) 2)
  (print (gethash "one" my-map)))
```

### Lisp (Lists)

Lisp uses lists for lists.

**Example:**

```lisp
(defstruct sexp
  (type nil)
  (value nil))

(defun make-list (children)
  (make-sexp :type 'list :value children))

(defmethod to-string ((sexp sexp))
  (if (eq (sexp-type sexp) 'atom)
      (sexp-value sexp)
      (format nil "(~{~a~})" (mapcar #'to-string (sexp-value sexp)))))
```

### Clojure (Maps)

Clojure uses maps for maps.

**Example:**

```clojure
(def my-map {"one" 1, "two" 2})
(println (get my-map "one"))
```

### Clojure (Lists)

Clojure uses lists for lists.

**Example:**

```clojure
(defn make-list [children]
  {:type :list :value children})
```

### F# (Maps)

F# uses `Map` for maps.

**Example:**

```fsharp
let myMap = Map.ofList [("one", 1); ("two", 2)]
printfn "%A" (myMap.["one"])
```

### F# (Lists)

F# uses lists for lists.

**Example:**

```fsharp
type SExp =
    | Atom of string
    | SList of SExp list

let rec toString sexp =
    match sexp with
    | Atom v -> v
    | SList l -> "(" + String.concat "" (List.map toString l) + ")"
```
</content>
</replace_in_file>
```
# Maps, Lists, and Vectors

### What are Maps?

Maps (also known as dictionaries or associative arrays) are data structures that store key-value pairs.

### Clojure

**Example:**

```clojure
(defn make-atom [value]
  {:type :atom :value value})
```

-   Here, a map is used to represent an atom, with keys `:type` and `:value`.

### What are Lists?

Lists are ordered collections of items.

### Clojure

**Example:**

```clojure
(defn make-list [children]
  {:type :list :value children})
```

-   Here, a list is used to represent the children of an S-expression.

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

-   Here, `Vec<SExp>` is used to store the children of an `SList`.

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
