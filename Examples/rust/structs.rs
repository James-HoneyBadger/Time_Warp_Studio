// Rust Structs and Enums Demo
// Time Warp Studio - Rust data types

struct Point {
    x: f64,
    y: f64,
}

struct Rectangle {
    width: f64,
    height: f64,
}

impl Rectangle {
    fn new(width: f64, height: f64) -> Rectangle {
        Rectangle { width: width, height: height }
    }

    fn area(&self) -> f64 {
        self.width * self.height
    }

    fn perimeter(&self) -> f64 {
        2.0 * (self.width + self.height)
    }

    fn is_square(&self) -> bool {
        self.width == self.height
    }
}

enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Triangle(f64, f64, f64),
}

fn shape_area(s: Shape) -> f64 {
    match s {
        Shape::Circle(r) => 3.14159 * r * r,
        Shape::Rect(w, h) => w * h,
        Shape::Triangle(a, b, c) => {
            let s = (a + b + c) / 2.0;
            (s * (s - a) * (s - b) * (s - c)).sqrt()
        }
    }
}

fn fibonacci(n: u32) -> u64 {
    if n <= 1 {
        return n as u64;
    }
    let mut a: u64 = 0;
    let mut b: u64 = 1;
    let mut i = 2;
    while i <= n {
        let temp = a + b;
        a = b;
        b = temp;
        i += 1;
    }
    b
}

fn main() {
    println!("=== Rust Structs and Enums ===\n");

    // Struct usage
    let r = Rectangle::new(5.0, 3.0);
    println!("Rectangle {}x{}", r.width, r.height);
    println!("  Area:      {}", r.area());
    println!("  Perimeter: {}", r.perimeter());
    println!("  Square?    {}", r.is_square());

    let sq = Rectangle::new(4.0, 4.0);
    println!("\nSquare {}x{}", sq.width, sq.height);
    println!("  Square?    {}", sq.is_square());

    // Enum and match
    println!("\nShape areas:");
    let shapes = vec![
        ("Circle r=3",       Shape::Circle(3.0)),
        ("Rect 4x6",         Shape::Rect(4.0, 6.0)),
        ("Triangle 3,4,5",   Shape::Triangle(3.0, 4.0, 5.0)),
    ];
    for (name, shape) in shapes {
        println!("  {}: {:.2}", name, shape_area(shape));
    }

    // Fibonacci
    println!("\nFibonacci sequence:");
    let mut i = 0;
    while i <= 10 {
        println!("  fib({}) = {}", i, fibonacci(i));
        i += 1;
    }
}
