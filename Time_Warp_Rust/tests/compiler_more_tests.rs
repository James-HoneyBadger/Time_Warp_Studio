use time_warp_unified::compiler::TempleCodeCompiler;

#[test]
fn compile_to_c_input_with_prompt() {
    let src = r#"
10 INPUT "Age?", A
20 PRINT A
30 END
"#;
    let comp = TempleCodeCompiler::new();
    let c = comp.compile_to_c(src).expect("compile_to_c failed");
    // Should emit prompt via fputs with the prompt string
    assert!(c.contains("fputs(\"Age? \", stdout);"));
    // Should read into buffers and parse double for A
    assert!(c.contains("read_line(INPUT_BUF"));
    assert!(c.contains("strncpy(S_A"));
    assert!(c.contains("V_A = strtod"));
}

#[test]
fn compile_to_c_print_numeric_literal() {
    let src = r#"
10 PRINT 42, " units"
20 END
"#;
    let comp = TempleCodeCompiler::new();
    let c = comp.compile_to_c(src).expect("compile_to_c failed");
    // Should print numeric literal directly
    assert!(c.contains("printf(\"%g\", 42)"));
    // And also print the string literal
    assert!(c.contains("fputs(\" units\""));
}

#[test]
fn compile_to_c_rem_comments() {
    let src = r#"
10 REM This is a comment
20 PRINT "Hello"
30 REM
40 END
"#;
    let comp = TempleCodeCompiler::new();
    let c = comp.compile_to_c(src).expect("compile_to_c failed");
    // REM should be converted to C comments
    assert!(c.contains("/* This is a comment */"));
    // Empty REM should also work
    assert!(c.contains("/*  */"));
    // Program should still work normally
    assert!(c.contains("fputs(\"Hello\""));
}
