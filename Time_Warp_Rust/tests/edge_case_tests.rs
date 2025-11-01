use time_warp_unified::graphics::TurtleState;
/// Edge case and robustness tests for Time Warp IDE
///
/// Tests parsing precedence, line number handling, name conflicts, and error recovery
use time_warp_unified::interpreter::Interpreter;

#[test]
fn test_basic_line_number_goto() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Test GOTO with actual line numbers (not just sequential)
    let program = r#"
10 PRINT "Start"
20 GOTO 40
30 PRINT "Should skip this"
40 PRINT "End"
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    assert!(output.iter().any(|s| s.contains("Start")));
    assert!(output.iter().any(|s| s.contains("End")));
    assert!(!output.iter().any(|s| s.contains("Should skip this")));
}

#[test]
fn test_basic_line_number_gosub_return() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Test GOSUB/RETURN with actual line numbers
    let program = r#"
10 PRINT "Main"
20 GOSUB 100
30 PRINT "After sub"
40 END
100 PRINT "In subroutine"
110 RETURN
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    assert!(output.iter().any(|s| s.contains("Main")));
    assert!(output.iter().any(|s| s.contains("In subroutine")));
    assert!(output.iter().any(|s| s.contains("After sub")));
}

#[test]
fn test_basic_line_number_if_then_goto() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Test IF...THEN with line number jump
    let program = r#"
10 LET X = 5
20 PRINT X
30 IF X > 3 THEN 60
40 PRINT "X is small"
50 GOTO 70
60 PRINT "X is big"
70 PRINT "Done"
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    println!("Output: {:?}", output);
    println!("X variable: {:?}", interp.variables.get("X"));

    assert!(output.iter().any(|s| s.contains("X is big")));
    assert!(!output.iter().any(|s| s.contains("X is small")));
    assert!(output.iter().any(|s| s.contains("Done")));
}

#[test]
fn test_logo_procedure_overrides_basic_keyword() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Define a Logo procedure with a name that could conflict with BASIC
    let program = r#"
TO PRINT :MSG
  FORWARD 50
  RIGHT 90
END

PRINT "test"
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    // Should succeed - Logo procedure should be called, not BASIC PRINT
    assert!(result.is_ok());
    // Turtle should have moved
    assert!(turtle.lines.len() > 0);
}

#[test]
fn test_case_insensitive_commands() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Test mixed case commands
    let program = r#"
forward 50
FoRwArD 30
FORWARD 20
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    assert!(result.is_ok());
    assert_eq!(turtle.lines.len(), 3); // Should have drawn 3 lines
}

#[test]
fn test_pilot_label_detection() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    let program = r#"
T:Start
J:SKIP
T:Should not see this
L:SKIP
T:After jump
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    assert!(output.iter().any(|s| s.contains("Start")));
    assert!(output.iter().any(|s| s.contains("After jump")));
    assert!(!output.iter().any(|s| s.contains("Should not see this")));
}

#[test]
fn test_mixed_line_numbers_and_no_numbers() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Mix numbered and unnumbered lines (common in educational settings)
    let program = r#"
10 PRINT "Line 10"
PRINT "Unnumbered"
20 PRINT "Line 20"
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    assert_eq!(output.len(), 3);
    assert!(output[0].contains("Line 10"));
    assert!(output[1].contains("Unnumbered"));
    assert!(output[2].contains("Line 20"));
}

#[test]
fn test_empty_lines_and_whitespace() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    let program = r#"
T:First

T:Second
    
    T:Third with leading spaces
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    // Should handle empty lines and leading whitespace gracefully
    assert_eq!(output.len(), 3);
}

#[test]
fn test_error_recovery_continues_execution() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Program with a bad line in the middle
    let program = r#"
10 PRINT "Before error"
20 GOTO 999
30 PRINT "After error"
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    // Should execute first line, report error, continue to third line
    assert!(output.iter().any(|s| s.contains("Before error")));
    assert!(output.iter().any(|s| s.contains("After error")));
    assert!(output
        .iter()
        .any(|s| s.contains("❌") || s.contains("Error")));
}

#[test]
fn test_screen_mode_switching() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    let program = r#"
10 SCREEN 0
20 PRINT "Text mode"
30 SCREEN 1
40 PRINT "Graphics 640x480"
50 SCREEN 2, 1024, 768
60 PRINT "Custom size"
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    assert!(result.is_ok());
    let output = result.unwrap();
    // Check that screen mode commands logged their changes
    assert!(output
        .iter()
        .any(|s| s.contains("Text") || s.contains("SCREEN")));
}

#[test]
fn test_cls_and_locate() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    let program = r#"
10 SCREEN 0
20 PRINT "Line 1"
30 PRINT "Line 2"
40 CLS
50 LOCATE 5, 10
60 PRINT "At row 5, col 10"
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    assert!(result.is_ok());
    // After CLS, text_lines should be cleared
    // Cursor should be at (4, 9) in 0-based coords after LOCATE 5, 10
}

#[test]
fn test_logo_repeat_nested() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Nested REPEAT commands
    let program = r#"
CLEARSCREEN
REPEAT 4 [FORWARD 100 REPEAT 3 [RIGHT 30 FORWARD 20] LEFT 90]
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    assert!(result.is_ok());
    // Should have drawn multiple line segments
    assert!(turtle.lines.len() > 10);
}

#[test]
fn test_basic_for_next_with_step() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    let program = r#"
10 FOR I = 10 TO 30 STEP 5
20 PRINT I
30 NEXT I
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    // Should print 10, 15, 20, 25, 30
    let numbers: Vec<i32> = output
        .iter()
        .filter_map(|s| s.trim().parse::<i32>().ok())
        .collect();

    assert_eq!(numbers, vec![10, 15, 20, 25, 30]);
}

#[test]
fn test_infinite_loop_protection() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Intentional infinite loop
    let program = r#"
10 GOTO 10
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    // Should terminate with max iterations warning
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output
        .iter()
        .any(|s| s.contains("Maximum iterations") || s.contains("⚠️")));
}
