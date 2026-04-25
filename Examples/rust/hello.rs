// Rust Hello World
// Time Warp Studio - Rust example

fn main() {
    println!("Hello, World!");
    println!("Rust: systems programming with memory safety.");

    // Variables and types
    let name = "Time Warp";
    let version: i32 = 10;
    println!("Welcome to {} Studio v{}!", name, version);

    // Arithmetic
    let sum: i32 = (1..=10).sum();
    println!("Sum of 1 to 10: {}", sum);

    // Vectors
    let languages = vec!["Rust", "Ruby", "Erlang", "Python", "Lua"];
    println!("\nLanguages:");
    for lang in &languages {
        println!("  - {}", lang);
    }

    // if/else
    let x = 42;
    if x > 100 {
        println!("\n{} is large", x);
    } else if x > 10 {
        println!("\n{} is medium", x);
    } else {
        println!("\n{} is small", x);
    }

    // match
    let day = 3;
    let day_name = match day {
        1 => "Monday",
        2 => "Tuesday",
        3 => "Wednesday",
        4 => "Thursday",
        5 => "Friday",
        _ => "Weekend",
    };
    println!("Day {}: {}", day, day_name);

    // Loop with accumulator
    let mut total = 0;
    for i in 1..=5 {
        total += i * i;
    }
    println!("\nSum of squares 1..5: {}", total);
}
