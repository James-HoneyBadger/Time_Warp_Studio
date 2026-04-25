# Rust Tutorial

## Introduction

Rust is a systems programming language focused on safety, speed, and concurrency. Created at Mozilla Research, Rust prevents memory safety bugs at compile time through its ownership system — without needing a garbage collector.

**File extension:** `.rs`

---

## Hello World

```rust
fn main() {
    println!("Hello, World!");
}
```

---

## Variables and Types

```rust
fn main() {
    let x = 5;            // immutable by default
    let mut y = 10;       // mutable
    y = 20;               // OK

    // Explicit types
    let a: i32 = 42;
    let b: f64 = 3.14;
    let c: bool = true;
    let d: &str = "hello";
    let e: String = String::from("world");

    println!("{} {} {} {} {}", a, b, c, d, e);

    // Constants
    const MAX_SIZE: u32 = 100;
    println!("Max size: {}", MAX_SIZE);
}
```

### Numeric Types

| Type  | Description           |
|-------|-----------------------|
| `i8`…`i128` | Signed integers |
| `u8`…`u128` | Unsigned integers |
| `f32`, `f64` | Floating-point |
| `isize`, `usize` | Pointer-sized |

---

## Control Flow

```rust
fn main() {
    let n = 7;

    // if/else if/else
    if n < 0 {
        println!("negative");
    } else if n == 0 {
        println!("zero");
    } else {
        println!("positive");
    }

    // if as expression
    let label = if n % 2 == 0 { "even" } else { "odd" };
    println!("{} is {}", n, label);
}
```

---

## Loops

```rust
fn main() {
    // loop (infinite, break to exit)
    let mut count = 0;
    let result = loop {
        count += 1;
        if count == 10 {
            break count * 2;  // loop returns a value
        }
    };
    println!("Result: {}", result);

    // while
    let mut n = 3;
    while n > 0 {
        println!("{}!", n);
        n -= 1;
    }

    // for..in
    for i in 1..=5 {
        println!("{}", i);
    }

    // iterate over a collection
    let v = vec![10, 20, 30];
    for x in &v {
        println!("{}", x);
    }
}
```

---

## Functions

```rust
fn add(a: i32, b: i32) -> i32 {
    a + b   // no semicolon = this is the return value
}

fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

fn main() {
    println!("{}", add(3, 4));
    println!("{}", greet("Alice"));
}
```

---

## Structs

```rust
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }

    fn distance_from_origin(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
    }
}

fn main() {
    let p = Point::new(3.0, 4.0);
    println!("Point: ({}, {})", p.x, p.y);
    println!("Distance: {}", p.distance_from_origin());
}
```

---

## Enums and Match

```rust
enum Direction {
    North,
    South,
    East,
    West,
}

fn describe(d: Direction) -> &'static str {
    match d {
        Direction::North => "going north",
        Direction::South => "going south",
        Direction::East  => "going east",
        Direction::West  => "going west",
    }
}

fn main() {
    println!("{}", describe(Direction::North));

    // Match with data
    let num = 7;
    let desc = match num {
        1         => "one",
        2 | 3     => "two or three",
        4..=9     => "four through nine",
        _         => "something else",
    };
    println!("{}: {}", num, desc);
}
```

---

## Option<T>

`Option<T>` represents a value that may or may not exist:

```rust
fn find_first_even(numbers: &[i32]) -> Option<i32> {
    for &n in numbers {
        if n % 2 == 0 {
            return Some(n);
        }
    }
    None
}

fn main() {
    let numbers = vec![1, 3, 5, 4, 7];
    match find_first_even(&numbers) {
        Some(n) => println!("Found even: {}", n),
        None    => println!("No even number"),
    }

    // unwrap_or: provide a default
    let first_even = find_first_even(&numbers).unwrap_or(-1);
    println!("First even or -1: {}", first_even);
}
```

---

## Vectors

```rust
fn main() {
    let mut v: Vec<i32> = Vec::new();
    v.push(1);
    v.push(2);
    v.push(3);

    // Macro shorthand
    let v2 = vec![10, 20, 30, 40, 50];

    println!("Length: {}", v2.len());
    println!("First: {:?}", v2.first());
    println!("Sum: {}", v2.iter().sum::<i32>());

    // Iteration
    for x in &v2 {
        print!("{} ", x);
    }
    println!();
}
```

---

## Closures

```rust
fn main() {
    let double = |x| x * 2;
    let add_n  = |x, n| x + n;

    println!("{}", double(5));
    println!("{}", add_n(10, 3));

    let numbers = vec![1, 2, 3, 4, 5];
    let doubled: Vec<i32> = numbers.iter().map(|&x| x * 2).collect();
    let evens:   Vec<i32> = numbers.iter().filter(|&&x| x % 2 == 0).cloned().collect();

    println!("{:?}", doubled);
    println!("{:?}", evens);
}
```

---

## String Types

```rust
fn main() {
    // &str: string slice (borrowed, immutable)
    let s1: &str = "hello";

    // String: owned heap string (mutable)
    let mut s2 = String::from("hello");
    s2.push_str(", world!");

    println!("{}", s1);
    println!("{}", s2);
    println!("Length: {}", s2.len());
    println!("Upper: {}", s2.to_uppercase());
    println!("Contains 'world': {}", s2.contains("world"));
}
```

---

## Turtle Graphics

```rust
fn main() {
    // Draw a square
    for _ in 0..4 {
        forward(100.0);
        right(90.0);
    }
}

// Or use namespace-style calls
fn main() {
    pencolor("blue");
    for i in 0..36 {
        forward(50.0);
        right(10.0);
    }
}
```

---

## Printing

```rust
fn main() {
    let name = "Alice";
    let n = 42;
    let pi = 3.14159;

    println!("Hello, {}!", name);        // Display trait
    println!("Debug: {:?}", vec![1,2]);  // Debug trait
    println!("Int: {}", n);
    println!("Float: {:.2}", pi);        // 2 decimal places
    println!("Padded: {:>10}", n);       // right-align in 10 chars
}
```

---

## Tips

- Variables are immutable by default; use `mut` for mutable ones
- Rust uses `{}` for Display formatting and `{:?}` for Debug
- `println!` adds a newline; `print!` does not
- The `match` expression must be exhaustive (cover all cases)
- `_` is the wildcard pattern (ignore/catch-all)
- Type suffixes: `42u32`, `3.14f64`, `100usize`
