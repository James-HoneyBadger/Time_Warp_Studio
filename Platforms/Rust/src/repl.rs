use std::io::{self, Write};
use crate::core::interpreter::Interpreter;
use crate::languages::basic::BasicInterpreter;
use crate::languages::pilot::PilotInterpreter;
use crate::languages::logo::LogoInterpreter;
use crate::languages::pascal::PascalInterpreter;
use crate::languages::forth::ForthInterpreter;
use crate::languages::prolog::PrologInterpreter;
use crate::languages::c::CInterpreter;

pub fn run() {
    println!("Time Warp Studio CLI");
    println!("Type 'exit' to quit, 'lang <name>' to switch language.");
    println!("Supported: BASIC, PILOT, LOGO, PASCAL, FORTH, PROLOG, C");
    println!("Current Language: BASIC");

    let mut basic = BasicInterpreter::new();
    let mut pilot = PilotInterpreter::new();
    let mut logo = LogoInterpreter::new();
    let mut pascal = PascalInterpreter::new();
    let mut forth = ForthInterpreter::new();
    let mut prolog = PrologInterpreter::new();
    let mut c_lang = CInterpreter::new();

    let mut current_lang = "BASIC";
    
    loop {
        print!("{} > ", current_lang);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => break, // EOF
            Ok(_) => {},
            Err(_) => break,
        }
        let trimmed = input.trim();
        
        if trimmed == "exit" || trimmed == "quit" {
            break;
        }
        
        if trimmed.starts_with("lang ") {
            let new_lang = trimmed[5..].trim().to_uppercase();
            match new_lang.as_str() {
                "BASIC" | "PILOT" | "LOGO" | "PASCAL" | "FORTH" | "PROLOG" | "C" => {
                    current_lang = Box::leak(new_lang.into_boxed_str()); // Leak to keep str reference simple for this loop
                    println!("Switched to {}", current_lang);
                },
                _ => println!("Unknown language: {}", new_lang),
            }
            continue;
        }

        // For some languages, we might want to support multi-line input in the future.
        // For now, we pass the single line.
        // Note: Some interpreters (like Pascal/C) expect full programs.
        // This REPL is best for BASIC, Forth, Logo, Prolog queries.
        
        let output = match current_lang {
            "BASIC" => basic.run(&input),
            "PILOT" => pilot.run(&input),
            "LOGO" => logo.run(&input),
            "PASCAL" => pascal.run(&input),
            "FORTH" => forth.run(&input),
            "PROLOG" => prolog.run(&input),
            "C" => c_lang.run(&input),
            _ => String::new(),
        };
        
        if !output.is_empty() {
            print!("{}", output);
        }
    }
}
