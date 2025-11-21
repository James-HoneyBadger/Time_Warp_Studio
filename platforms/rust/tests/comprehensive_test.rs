use time_warp_unified::graphics::TurtleState;
/// Comprehensive integration test demonstrating all fixed features
use time_warp_unified::interpreter::Interpreter;

#[test]
fn test_comprehensive_all_features() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Test program with:
    // - BASIC line numbers and GOTO/GOSUB
    // - Comparison operators in IF statements
    // - Logo graphics
    // - PILOT output
    // - SCREEN mode switching
    let program = r#"
T:=== Time Warp Comprehensive Test ===

10 REM Test BASIC with line numbers
20 LET X = 10
30 IF X > 5 THEN GOSUB 100
40 PRINT "Back from subroutine"
50 GOTO 200

100 REM Subroutine
110 PRINT "In subroutine, X ="; X
120 RETURN

200 REM Test Logo graphics
CLEARSCREEN
REPEAT 4 [FORWARD 50 RIGHT 90]
HOME

300 REM Test screen mode
SCREEN 1
PRINT "Graphics mode 640x480"

T:Test complete!
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    assert!(result.is_ok());
    let output = result.unwrap();

    // Verify all parts executed
    assert!(output
        .iter()
        .any(|s| s.contains("Time Warp Comprehensive Test")));
    assert!(output.iter().any(|s| s.contains("In subroutine")));
    assert!(output.iter().any(|s| s.contains("Back from subroutine")));
    assert!(output.iter().any(|s| s.contains("Graphics mode")));
    assert!(output.iter().any(|s| s.contains("Test complete")));

    // Verify turtle drew a square
    assert!(
        turtle.lines.len() >= 4,
        "Should have drawn at least 4 lines for square"
    );
}

#[test]
fn test_comparison_operators_all() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    let program = r#"
10 LET A = 10
20 LET B = 5
30 IF A > B THEN PRINT "A > B: true"
40 IF A < B THEN PRINT "A < B: false"
50 IF A >= 10 THEN PRINT "A >= 10: true"
60 IF B <= 5 THEN PRINT "B <= 5: true"
70 IF A == 10 THEN PRINT "A == 10: true"
80 IF B != 10 THEN PRINT "B != 10: true"
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    // Should have 5 true statements (lines 30, 50, 60, 70, 80)
    let true_count = output.iter().filter(|s| s.contains("true")).count();
    assert_eq!(true_count, 5, "Expected 5 true comparisons");
}

#[test]
fn test_logo_procedure_precedence() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Define Logo procedure that shadows BASIC keyword
    let program = r#"
TO FORWARD :N
  REPEAT :N [FD 10 RIGHT 10]
END

FORWARD 5
"#;

    interp.load_program(program).unwrap();
    let result = interp.execute(&mut turtle);

    assert!(result.is_ok());
    // Custom FORWARD should have drawn multiple small segments
    assert!(turtle.lines.len() >= 5);
}

#[test]
fn test_line_number_mapping_sparse() {
    let mut interp = Interpreter::new();
    let mut turtle = TurtleState::default();

    // Sparse line numbers (like old BASIC programs)
    let program = r#"
10 PRINT "Start"
100 PRINT "Hundred"  
1000 PRINT "Thousand"
10000 GOTO 100000
50000 PRINT "Skip this"
100000 PRINT "End"
"#;

    interp.load_program(program).unwrap();
    let output = interp.execute(&mut turtle).unwrap();

    assert!(output.iter().any(|s| s.contains("Start")));
    assert!(output.iter().any(|s| s.contains("Hundred")));
    assert!(output.iter().any(|s| s.contains("Thousand")));
    assert!(output.iter().any(|s| s.contains("End")));
    assert!(!output.iter().any(|s| s.contains("Skip this")));
}
