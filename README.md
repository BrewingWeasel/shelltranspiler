# Shush

Shush is a typed language that can be transpiled to posix shell scripts

The project is still very much a work in progress and there will be lots of bugs

Here is a basic sample:

```sh
# loops work as expected
var fruits = ["apple", "banana", "orange"]
push(fruits, "grape")

for fruit in fruits {
  @print("{}", fruit)
}

# functions are defined with the fun keyword
# they can be prefaced with pub to make them public
fun add(a: int, b: int) -> int {
  return a + b
}
var sum = add(1, 2) # => 3

# default values can be specified after a pipe: |
fun add_default(a: int | b=1) -> int {
  return a + b
}

value = add_default(1) # => 2
@print("value is {}", value)
value = add_default(1 | b=2) # => 3
@print("value is {}", value)

# (this is also used in prelude functions such as pop)

# generics are supported
fun get_first<v>(x: [%v]) -> %v {
  return x[1]
}

# types can be inferred
var first_num = get_first([1])

# or specified
var first_number: string = get_first(["one"])

# this would be a compilation error:
#var other_first_num: bool = get_first([1])

# Algebraic types/rust style enums are supported
enum Option<v> {
  Some(%v),
  None()
}

# the type can be inferred
var val = Option::Some(true)

# or specified if there is not enough information
var other_val = Option<v: int>::None()

if let Option::Some(value) = val {
  # if the following line was uncommented, it would be a compilation error
  # var value: int = val
  @print("the value is {}", value)
}

if let Option::Some(value) = other_val {
  @print("this will never be printed")
}

# You can use "macros" to get finer control

var cat = "kitty"
var output = @stdout(@eval("echo", "the value is", @raw_name(cat)))

@print("output: {}", output) # => output: the value is kitty

var str_val: string = @unsafe_into(5)

# You can import and use function from other files or the standard library
import term
@print("the color is {}", term.yellow("yellow"))
term.cursor_up(1)

# Macros such as @print can take variable arguments and format them, similar to println! in rust
@print("Hi there {}", input("what is your name? "))
```
