package basic

import (
	"strings"
	"testing"
)

// TestExecutor_InputCommand tests INPUT command
func TestExecutor_InputCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("INPUT X")
	if err != nil {
		t.Fatalf("Execute(INPUT) error = %v", err)
	}
	if !strings.Contains(result, "📝") {
		t.Errorf("INPUT should contain input emoji: %q", result)
	}
}

// TestExecutor_GotoCommand tests GOTO in single command mode
func TestExecutor_GotoCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("GOTO 100")
	if err != nil {
		t.Fatalf("Execute(GOTO) error = %v", err)
	}
	if !strings.Contains(result, "🚀") {
		t.Errorf("GOTO should contain jump emoji: %q", result)
	}
}

// TestExecutor_IfCommand tests IF in single command mode
func TestExecutor_IfCommand(t *testing.T) {
	e := New()
	e.variables["X"] = 1
	result, err := e.Execute("IF X THEN PRINT \"YES\"")
	if err != nil {
		t.Fatalf("Execute(IF) error = %v", err)
	}
	// IF returns message when condition is true (non-zero)
	if !strings.Contains(result, "✅") {
		t.Errorf("IF should show condition result: %q", result)
	}

	// Test IF with false condition (zero)
	e.variables["X"] = 0
	result2, err := e.Execute("IF X THEN PRINT \"YES\"")
	if err != nil {
		t.Fatalf("Execute(IF false) error = %v", err)
	}
	if result2 != "" {
		t.Errorf("IF with false condition should return empty: %q", result2)
	}
}

// TestExecutor_ForCommand tests FOR in single command mode
func TestExecutor_ForCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("FOR I = 1 TO 10")
	if err != nil {
		t.Fatalf("Execute(FOR) error = %v", err)
	}
	if !strings.Contains(result, "🔄") {
		t.Errorf("FOR should succeed: %q", result)
	}
}

// TestExecutor_NextCommand tests NEXT in single command mode
func TestExecutor_NextCommand(t *testing.T) {
	e := New()
	// Set up a FOR loop first
	e.forStack = append(e.forStack, forContext{
		variable:  "I",
		endVal:    5,
		step:      1,
		startLine: 0,
	})
	e.variables["I"] = 1

	result, err := e.Execute("NEXT I")
	if err != nil {
		t.Fatalf("Execute(NEXT) error = %v", err)
	}
	if !strings.Contains(result, "🔄") {
		t.Errorf("NEXT should succeed: %q", result)
	}
}

// TestExecutor_GosubCommand tests GOSUB in single command mode
func TestExecutor_GosubCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("GOSUB 100")
	if err != nil {
		t.Fatalf("Execute(GOSUB) error = %v", err)
	}
	if !strings.Contains(result, "🚀") {
		t.Errorf("GOSUB should contain jump emoji: %q", result)
	}
}

// TestExecutor_ReturnCommand tests RETURN in single command mode
func TestExecutor_ReturnCommand(t *testing.T) {
	e := New()
	// Set up a GOSUB stack
	e.gosubStack = append(e.gosubStack, 10)

	result, err := e.Execute("RETURN")
	if err != nil {
		t.Fatalf("Execute(RETURN) error = %v", err)
	}
	if !strings.Contains(result, "🔙") {
		t.Errorf("RETURN should succeed: %q", result)
	}
}

// TestExecutor_ErrorHandling tests various error conditions
func TestExecutor_ErrorHandling(t *testing.T) {
	e := New()

	// RETURN without GOSUB
	e.gosubStack = []int{}
	result, _ := e.Execute("RETURN")
	if !strings.Contains(result, "❌") {
		t.Errorf("RETURN without GOSUB should error: %q", result)
	}

	// NEXT without FOR
	e.forStack = []forContext{}
	result, _ = e.Execute("NEXT I")
	if !strings.Contains(result, "❌") {
		t.Errorf("NEXT without FOR should error: %q", result)
	}

	// IF without THEN
	result, _ = e.Execute("IF X > 5")
	if !strings.Contains(result, "❌") {
		t.Errorf("IF without THEN should error: %q", result)
	}

	// FOR without TO
	result, _ = e.Execute("FOR I = 1")
	if !strings.Contains(result, "❌") {
		t.Errorf("FOR without TO should error: %q", result)
	}

	// FOR without =
	result, _ = e.Execute("FOR I TO 10")
	if !strings.Contains(result, "❌") {
		t.Errorf("FOR without = should error: %q", result)
	}
}

// TestExecutor_EvalNumeric tests numeric expression evaluation edge cases
func TestExecutor_EvalNumeric(t *testing.T) {
	e := New()
	e.variables["X"] = 42
	e.variables["Y"] = 8

	tests := []struct {
		name     string
		expr     string
		expected float64
	}{
		{"variable", "X", 42},
		{"addition", "X + Y", 50},
		{"subtraction", "X - Y", 34},
		{"multiplication", "X * 2", 84},
		{"division", "Y / 2", 4},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := e.evalNumeric(tt.expr)
			if result != tt.expected {
				t.Errorf("evalNumeric(%q) = %v, want %v", tt.expr, result, tt.expected)
			}
		})
	}
}

// TestExecutor_ComplexProgram tests a comprehensive BASIC program
func TestExecutor_ComplexProgram(t *testing.T) {
	e := New()
	program := `
10 LET SUM = 0
20 FOR I = 1 TO 5
30 LET SUM = SUM + I
40 NEXT I
50 PRINT SUM
60 END
`
	result := e.RunProgram(program)

	// Sum of 1+2+3+4+5 = 15
	if !strings.Contains(result, "15") {
		t.Errorf("Complex program should compute sum as 15: %q", result)
	}
}

// TestExecutor_EmptyCommand tests empty command handling
func TestExecutor_EmptyCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("")
	if err != nil {
		t.Fatalf("Execute('') should not error: %v", err)
	}
	if result != "" {
		t.Errorf("Empty command should return empty string: %q", result)
	}
}

// TestExecutor_LineCircleLocateErrors tests error paths for graphics commands
func TestExecutor_LineCircleLocateErrors(t *testing.T) {
	e := New()

	// LINE with insufficient args
	result, _ := e.Execute("LINE")
	if !strings.Contains(result, "❌") {
		t.Errorf("LINE without args should error: %q", result)
	}

	result, _ = e.Execute("LINE 1,2")
	if !strings.Contains(result, "❌") {
		t.Errorf("LINE with insufficient args should error: %q", result)
	}

	// CIRCLE with insufficient args
	result, _ = e.Execute("CIRCLE")
	if !strings.Contains(result, "❌") {
		t.Errorf("CIRCLE without args should error: %q", result)
	}

	result, _ = e.Execute("CIRCLE 1")
	if !strings.Contains(result, "❌") {
		t.Errorf("CIRCLE with insufficient args should error: %q", result)
	}

	// LOCATE with insufficient args
	result, _ = e.Execute("LOCATE")
	if !strings.Contains(result, "❌") {
		t.Errorf("LOCATE without args should error: %q", result)
	}

	result, _ = e.Execute("LOCATE 1")
	if !strings.Contains(result, "❌") {
		t.Errorf("LOCATE with insufficient args should error: %q", result)
	}
}

// TestExecutor_PrintEdgeCases tests PRINT with various inputs
func TestExecutor_PrintEdgeCases(t *testing.T) {
	e := New()
	e.variables["NAME"] = 100

	// PRINT variable
	result, err := e.Execute("PRINT NAME")
	if err != nil {
		t.Fatalf("Execute(PRINT NAME) error = %v", err)
	}
	if !strings.Contains(result, "100") {
		t.Errorf("PRINT NAME should output 100: %q", result)
	}

	// PRINT with no argument
	result, err = e.Execute("PRINT")
	if err != nil {
		t.Fatalf("Execute(PRINT) error = %v", err)
	}
	// Should print newline/empty
}

// TestExecutor_LetErrors tests LET error handling
func TestExecutor_LetErrors(t *testing.T) {
	e := New()

	// LET without =
	result, _ := e.Execute("LET X 10")
	if !strings.Contains(result, "❌") {
		t.Errorf("LET without = should error: %q", result)
	}
}

// TestExecutor_RunProgramEdgeCases tests RunProgram edge cases
func TestExecutor_RunProgramEdgeCases(t *testing.T) {
	e := New()

	// Empty program
	result := e.RunProgram("")
	if result != "" {
		t.Errorf("Empty program should produce empty output: %q", result)
	}

	// Program with only whitespace
	result = e.RunProgram("   \n  \n  ")
	if result != "" {
		t.Errorf("Whitespace-only program should produce empty output: %q", result)
	}

	// Program with GOTO to undefined line
	program := `
10 GOTO 999
20 PRINT "NEVER"
`
	result = e.RunProgram(program)
	if !strings.Contains(result, "❌") {
		t.Errorf("GOTO to undefined line should error: %q", result)
	}

	// Program exceeding max steps (infinite loop protection)
	infiniteLoop := `
10 GOTO 10
`
	result = e.RunProgram(infiniteLoop)
	// Should terminate without hanging
}

// TestExecutor_ForNextWithStep tests FOR/NEXT with various steps
func TestExecutor_ForNextWithStep(t *testing.T) {
	e := New()

	// Negative step
	program := `
10 FOR I = 10 TO 1 STEP -1
20 PRINT I
30 NEXT I
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "10") {
		t.Errorf("FOR with negative STEP should work: %q", result)
	}

	// Fractional step
	program2 := `
10 FOR I = 0 TO 2 STEP 0.5
20 PRINT I
30 NEXT I
`
	result2 := e.RunProgram(program2)
	// Should iterate with fractional steps
	_ = result2
}

// TestExecutor_PrintStringLiteral tests PRINT with quoted strings
func TestExecutor_PrintStringLiteral(t *testing.T) {
	e := New()

	result, err := e.Execute(`PRINT "Hello World"`)
	if err != nil {
		t.Fatalf("Execute(PRINT string) error = %v", err)
	}
	if !strings.Contains(result, "Hello World") {
		t.Errorf("PRINT should output string literal: %q", result)
	}
}

// TestExecutor_EvaluateExpression tests evaluateExpression function
func TestExecutor_EvaluateExpression(t *testing.T) {
	e := New()

	// Pure numeric
	result := e.evaluateExpression("42")
	if result != "42" {
		t.Errorf("evaluateExpression(42) = %q, want '42'", result)
	}

	// String literal
	result = e.evaluateExpression(`"test"`)
	if result != "test" {
		t.Errorf("evaluateExpression(string) = %q, want 'test'", result)
	}
}

// TestExecutor_RunProgramGosubReturn tests GOSUB/RETURN in programs
func TestExecutor_RunProgramGosubReturn(t *testing.T) {
	e := New()
	program := `
10 GOSUB 100
20 PRINT "BACK"
30 END
100 PRINT "SUB"
110 RETURN
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "SUB") || !strings.Contains(result, "BACK") {
		t.Errorf("GOSUB/RETURN program should work: %q", result)
	}
}

// TestExecutor_RunProgramIfGoto tests IF...THEN with GOTO
func TestExecutor_RunProgramIfGoto(t *testing.T) {
	e := New()
	program := `
10 LET X = 5
20 IF X THEN 50
30 PRINT "SKIP"
40 END
50 PRINT "JUMP"
60 END
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "JUMP") {
		t.Errorf("IF...THEN GOTO should jump: %q", result)
	}
	if strings.Contains(result, "SKIP") {
		t.Errorf("IF...THEN GOTO should skip middle: %q", result)
	}
}

// TestExecutor_RunProgramIfWithoutNumber tests IF...THEN without line number
func TestExecutor_RunProgramIfWithoutNumber(t *testing.T) {
	e := New()
	program := `
10 LET X = 1
20 IF X THEN END
30 PRINT "AFTER"
`
	result := e.RunProgram(program)
	// IF with non-numeric THEN just continues
	_ = result
}

// TestExecutor_RunProgramForErrors tests FOR loop error paths
func TestExecutor_RunProgramForErrors(t *testing.T) {
	e := New()

	// FOR without =
	program := `
10 FOR I TO 10
20 NEXT I
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "❌") {
		t.Errorf("FOR without = should error: %q", result)
	}

	// FOR without TO
	program2 := `
10 FOR I = 1
20 NEXT I
`
	result2 := e.RunProgram(program2)
	if !strings.Contains(result2, "❌") {
		t.Errorf("FOR without TO should error: %q", result2)
	}
}

// TestExecutor_RunProgramGotoError tests GOTO error path
func TestExecutor_RunProgramGotoError(t *testing.T) {
	e := New()
	program := `
10 GOTO INVALID
20 PRINT "OK"
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "❌") {
		t.Errorf("GOTO with invalid arg should error: %q", result)
	}
}

// TestExecutor_RunProgramIfError tests IF error paths
func TestExecutor_RunProgramIfError(t *testing.T) {
	e := New()
	program := `
10 IF 1
20 PRINT "OK"
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "❌") {
		t.Errorf("IF without THEN should error: %q", result)
	}
}

// TestExecutor_RunProgramInputInBatch tests INPUT in program mode
func TestExecutor_RunProgramInputInBatch(t *testing.T) {
	e := New()
	program := `
10 INPUT X
20 PRINT X
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "📝") {
		t.Errorf("INPUT in program should prompt: %q", result)
	}
}

// TestExecutor_RunProgramStepVariations tests STEP edge cases
func TestExecutor_RunProgramStepVariations(t *testing.T) {
	e := New()

	// FOR with explicit STEP 1
	program := `
10 FOR I = 1 TO 3 STEP 1
20 PRINT I
30 NEXT I
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "1") || !strings.Contains(result, "3") {
		t.Errorf("FOR with STEP 1 should work: %q", result)
	}

	// FOR with STEP 2
	program2 := `
10 FOR I = 0 TO 4 STEP 2
20 PRINT I
30 NEXT I
`
	result2 := e.RunProgram(program2)
	if !strings.Contains(result2, "0") || !strings.Contains(result2, "4") {
		t.Errorf("FOR with STEP 2 should work: %q", result2)
	}
}

// TestExecutor_RunProgramCLS tests CLS in program
func TestExecutor_RunProgramCLS(t *testing.T) {
	e := New()
	program := `
10 CLS
20 PRINT "CLEAR"
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "CLEAR") {
		t.Errorf("CLS program should work: %q", result)
	}
}

// TestExecutor_RunProgramAssignmentVariations tests assignment variations
func TestExecutor_RunProgramAssignmentVariations(t *testing.T) {
	e := New()

	// Assignment without LET
	program := `
10 X = 42
20 PRINT X
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "42") {
		t.Errorf("Assignment without LET should work: %q", result)
	}

	// Assignment with LET
	program2 := `
10 LET Y = 99
20 PRINT Y
`
	result2 := e.RunProgram(program2)
	if !strings.Contains(result2, "99") {
		t.Errorf("Assignment with LET should work: %q", result2)
	}
}

// TestExecutor_RunProgramRemAndEmpty tests REM and empty lines
func TestExecutor_RunProgramRemAndEmpty(t *testing.T) {
	e := New()
	program := `
10 REM This is a comment

20 PRINT "OK"
30 REM Another comment
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "OK") {
		t.Errorf("Program with REM should work: %q", result)
	}
}

// TestExecutor_SingleCommandGotoError covers executeGoto error path
func TestExecutor_SingleCommandGotoError(t *testing.T) {
	e := New()
	result, err := e.Execute("GOTO ABC")
	if err != nil {
		t.Fatalf("Execute(GOTO ABC) unexpected error: %v", err)
	}
	if !strings.Contains(result, "❌") {
		t.Errorf("GOTO with non-numeric should error: %q", result)
	}
}

// TestExecutor_ExecuteUnknownAssignment tests implicit LET via assignment branch
func TestExecutor_ExecuteUnknownAssignment(t *testing.T) {
	e := New()
	out, err := e.Execute("X = 123")
	if err != nil {
		t.Fatalf("Execute implicit assignment error: %v", err)
	}
	if out != "" || e.variables["X"] != 123 {
		t.Errorf("Implicit assignment failed: out=%q X=%v", out, e.variables["X"])
	}
}

// TestExecutor_ExecuteUnknownCommand tests default unknown command branch
func TestExecutor_ExecuteUnknownCommand(t *testing.T) {
	e := New()
	out, err := e.Execute("ZZZ")
	if err != nil {
		t.Fatalf("Execute unknown command error: %v", err)
	}
	if !strings.Contains(out, "unknown command") {
		t.Errorf("Expected unknown command message: %q", out)
	}
}

// TestExecutor_ExecuteReturnEmptyStack tests RETURN with empty gosub stack in single-command mode
func TestExecutor_ExecuteReturnEmptyStack(t *testing.T) {
	e := New()
	out, err := e.Execute("RETURN")
	if err != nil {
		t.Fatalf("Execute RETURN error: %v", err)
	}
	if !strings.Contains(out, "❌") {
		t.Errorf("Expected error for empty RETURN: %q", out)
	}
}

// TestExecutor_ExecuteEnd tests END command output
func TestExecutor_ExecuteEnd(t *testing.T) {
	e := New()
	out, _ := e.Execute("END")
	if !strings.Contains(out, "Program ended") {
		t.Errorf("END did not produce termination message: %q", out)
	}
}

// TestExecutor_ExecuteCls tests CLS command output
func TestExecutor_ExecuteCls(t *testing.T) {
	e := New()
	out, _ := e.Execute("CLS")
	if !strings.Contains(out, "Screen cleared") {
		t.Errorf("CLS did not produce clear message: %q", out)
	}
}

// TestExecutor_ExecuteEcho covers ECHO branch
func TestExecutor_ExecuteEcho(t *testing.T) {
	e := New()
	out, _ := e.Execute("ECHO hello world")
	if !strings.Contains(out, "✅ hello world") {
		t.Errorf("ECHO did not echo message: %q", out)
	}
}

// TestExecutor_ExecuteLetWithoutSpaces covers implicit LET with spaces
func TestExecutor_ExecuteLetWithoutSpaces(t *testing.T) {
	e := New()
	out, _ := e.Execute("  A   =   5  ")
	if out != "" || e.variables["A"] != 5 {
		t.Errorf("Implicit LET with spaces failed: out=%q A=%v", out, e.variables["A"])
	}
}

// TestExecutor_RunProgramGotoNegativeUnknown covers GOTO to negative/unknown
func TestExecutor_RunProgramGotoNegativeUnknown(t *testing.T) {
	e := New()
	program := `
10 GOTO -1
20 PRINT "AFTER"
30 END
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "Unknown line") {
		t.Errorf("GOTO negative should be unknown: %q", out)
	}
}

// TestExecutor_RunProgramForParseErrors covers FOR parse error branches
func TestExecutor_RunProgramForParseErrors(t *testing.T) {
	e := New()
	programMissingTo := `
10 FOR I = 1 5
20 PRINT "X"
30 END
`
	r1 := e.RunProgram(programMissingTo)
	if !strings.Contains(r1, "FOR requires TO") {
		t.Errorf("Missing TO not detected: %q", r1)
	}

	e2 := New()
	programMissingEquals := `
10 FOR I 1 TO 5
20 PRINT "X"
30 END
`
	r2 := e2.RunProgram(programMissingEquals)
	if !strings.Contains(r2, "FOR requires var = start TO end") {
		t.Errorf("Missing '=' not detected: %q", r2)
	}
}

// TestExecutor_ExecuteForWithStep covers executeFor STEP parsing branch
func TestExecutor_ExecuteForWithStep(t *testing.T) {
	e := New()
	out, err := e.Execute("FOR J = 5 TO 1 STEP -2")
	if err != nil {
		t.Fatalf("Execute FOR with STEP error: %v", err)
	}
	if !strings.Contains(out, "🔄 FOR J = 5") || !strings.Contains(out, "STEP -2") {
		t.Errorf("FOR with STEP not parsed correctly: %q", out)
	}
}

// TestExecutor_RunProgramGosubUnknown covers unknown line after GOSUB
func TestExecutor_RunProgramGosubUnknown(t *testing.T) {
	e := New()
	program := `
10 GOSUB 9999
20 PRINT "AFTER"
30 END
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "Unknown line") {
		t.Errorf("Expected unknown line error for GOSUB: %q", out)
	}
}

// TestExecutor_RunProgramReturnWithoutGosub covers RETURN error inside program
func TestExecutor_RunProgramReturnWithoutGosub(t *testing.T) {
	e := New()
	program := `
10 RETURN
20 PRINT "AFTER"
30 END
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "RETURN without GOSUB") {
		t.Errorf("RETURN without GOSUB not reported: %q", out)
	}
}

// TestExecutor_RunProgramGraphics covers LINE, CIRCLE, LOCATE branches
func TestExecutor_RunProgramGraphics(t *testing.T) {
	e := New()
	program := `
10 LINE 1,2,3,4
20 CIRCLE 10,10,5
30 LOCATE 5,7
40 END
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "LINE 1,2,3,4") || !strings.Contains(out, "CIRCLE 10,10,5") || !strings.Contains(out, "LOCATE 5,7") {
		t.Errorf("Graphics commands missing in output: %q", out)
	}
}

// TestExecutor_ExecuteRem covers REM comment branch in single-command mode
func TestExecutor_ExecuteRem(t *testing.T) {
	e := New()
	out, err := e.Execute("REM this is a comment")
	if err != nil {
		t.Fatalf("Execute REM error: %v", err)
	}
	if out != "" {
		t.Errorf("REM should produce no output: %q", out)
	}
}

// TestExecutor_RunProgramIfInvalidTarget covers IF with non-existent numeric target
func TestExecutor_RunProgramIfInvalidTarget(t *testing.T) {
	e := New()
	program := `
10 LET X = 1
20 IF X THEN 9999
30 PRINT "AFTER"
40 END
`
	out := e.RunProgram(program)
	// Should not jump; should print AFTER
	if !strings.Contains(out, "AFTER") {
		t.Errorf("IF invalid target should continue: %q", out)
	}
}

// TestExecutor_SingleCommandGosubError covers executeGosub error path
func TestExecutor_SingleCommandGosubError(t *testing.T) {
	e := New()
	result, err := e.Execute("GOSUB xyz")
	if err != nil {
		t.Fatalf("Execute(GOSUB xyz) unexpected error: %v", err)
	}
	if !strings.Contains(result, "❌") {
		t.Errorf("GOSUB with non-numeric should error: %q", result)
	}
}

// TestExecutor_NextCompletion tests NEXT loop completion (✅ branch)
func TestExecutor_NextCompletion(t *testing.T) {
	e := New()
	e.forStack = append(e.forStack, forContext{variable: "I", endVal: 5, step: 1, startLine: 0})
	e.variables["I"] = 5 // already at end, NEXT should immediately complete
	result, err := e.Execute("NEXT I")
	if err != nil {
		t.Fatalf("Execute(NEXT I) error: %v", err)
	}
	if !strings.Contains(result, "✅") {
		t.Errorf("Expected loop completion ✅: %q", result)
	}
}

// TestExecutor_NextNegativeStepCompletion tests negative step completion branch
func TestExecutor_NextNegativeStepCompletion(t *testing.T) {
	e := New()
	// Loop I = 0 TO -5 STEP -1 ; set current at -5 so next decrements past end
	e.forStack = append(e.forStack, forContext{variable: "I", endVal: -5, step: -1, startLine: 0})
	e.variables["I"] = -5
	result, err := e.Execute("NEXT I")
	if err != nil {
		t.Fatalf("Execute(NEXT I) error: %v", err)
	}
	if !strings.Contains(result, "✅") {
		t.Errorf("Expected negative step completion ✅: %q", result)
	}
}

// TestExecutor_EvalNumericAdvanced covers ABS/INT/SQR/div by zero/negative
func TestExecutor_EvalNumericAdvanced(t *testing.T) {
	e := New()
	e.variables["NEG"] = -7
	cases := []struct {
		expr string
		want float64
	}{
		{"ABS(5)", 5},
		{"INT(3.9)", 3},
		{"SQR(9)", 3},
		{"NEG", -7},
		{"10/0", 0}, // division by zero returns 0 per implementation
	}
	for _, c := range cases {
		got := e.evalNumeric(c.expr)
		if got != c.want {
			t.Errorf("evalNumeric(%s)=%v want %v", c.expr, got, c.want)
		}
	}
}

// TestExecutor_RunProgramUnknownCommand covers default unknown branch
func TestExecutor_RunProgramUnknownCommand(t *testing.T) {
	e := New()
	program := `
10 FOO 123
20 END
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "unknown command") {
		t.Errorf("Unknown command branch not hit: %q", result)
	}
}

// TestExecutor_RunProgramLineOnly covers lines with just a number
func TestExecutor_RunProgramLineOnly(t *testing.T) {
	e := New()
	program := `
10
20 PRINT "OK"
30
40 END
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "OK") {
		t.Errorf("Line-only program should still run PRINT: %q", result)
	}
}

// TestExecutor_RunProgramMaxSteps asserts infinite loop guard message
func TestExecutor_RunProgramMaxSteps(t *testing.T) {
	e := New()
	program := `
10 GOTO 10
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "too many steps") {
		t.Errorf("Expected max steps guard message: %q", result)
	}
}

// TestExecutor_RunProgramMismatchedNext covers NEXT with wrong variable name (ignored)
func TestExecutor_RunProgramMismatchedNext(t *testing.T) {
	e := New()
	program := `
10 FOR I = 1 TO 2
20 NEXT J
30 PRINT "OK"
40 END
`
	result := e.RunProgram(program)
	if !strings.Contains(result, "OK") {
		t.Errorf("Mismatched NEXT should be ignored and program continue: %q", result)
	}
}
