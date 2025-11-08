package pilot

import (
	"strings"
	"testing"
)

// TestExecutor_Type tests T: (type/display) command
func TestExecutor_Type(t *testing.T) {
	tests := []struct {
		name     string
		command  string
		expected string
	}{
		{
			name:     "simple text",
			command:  "T:Hello, World!",
			expected: "Hello, World!",
		},
		{
			name:     "empty text",
			command:  "T:",
			expected: "",
		},
		{
			name:     "text with spaces",
			command:  "T:  Welcome to PILOT  ",
			expected: "Welcome to PILOT",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := New()
			result, err := e.Execute(tt.command)
			if err != nil {
				t.Fatalf("Execute() error = %v", err)
			}
			if !strings.Contains(result, tt.expected) {
				t.Errorf("Execute() = %q, want to contain %q", result, tt.expected)
			}
		})
	}
}

// TestExecutor_Use tests U: (use/assign) command
func TestExecutor_Use(t *testing.T) {
	e := New()

	// Assign a variable (returns empty string on success)
	_, err := e.Execute("U:NAME=John")
	if err != nil {
		t.Fatalf("Execute(U:NAME=John) error = %v", err)
	}

	// Verify variable was set
	if val, ok := e.variables["NAME"]; !ok || val != "John" {
		t.Errorf("Variable NAME = %q, want 'John'", val)
	}

	// Assign numeric value
	_, err = e.Execute("U:AGE=25")
	if err != nil {
		t.Fatalf("Execute(U:AGE=25) error = %v", err)
	}
	if val, ok := e.variables["AGE"]; !ok || val != "25" {
		t.Errorf("Variable AGE = %q, want '25'", val)
	}
}

// TestExecutor_Accept tests A: (accept input) command
func TestExecutor_Accept(t *testing.T) {
	e := New()

	// Accept command should prompt for input
	result, err := e.Execute("A:USERNAME")
	if err != nil {
		t.Fatalf("Execute(A:USERNAME) error = %v", err)
	}
	if !strings.Contains(result, "📝") {
		t.Errorf("A: should contain input emoji: %q", result)
	}
}

// TestExecutor_Compute tests C: (compute/test condition) command
func TestExecutor_Compute(t *testing.T) {
	e := New()
	e.variables["X"] = "10"
	e.variables["Y"] = "20"

	// Test condition that should be true (returns empty string)
	_, err := e.Execute("C:X<Y")
	if err != nil {
		t.Fatalf("Execute(C:X<Y) error = %v", err)
	}
	if !e.conditionResult {
		t.Error("C:X<Y should set conditionResult to true")
	}

	// Test condition that should be false (returns empty string)
	_, err = e.Execute("C:X>Y")
	if err != nil {
		t.Fatalf("Execute(C:X>Y) error = %v", err)
	}
	if e.conditionResult {
		t.Error("C:X>Y should set conditionResult to false")
	}
}

// TestExecutor_YesNo tests Y: (yes) and N: (no) conditional commands
func TestExecutor_YesNo(t *testing.T) {
	e := New()

	// Set condition to true
	e.conditionResult = true

	// Y: should execute when condition is true (but returns fixed message)
	result, err := e.Execute("Y:T:Condition is true")
	if err != nil {
		t.Fatalf("Execute(Y:) error = %v", err)
	}
	if !strings.Contains(result, "YES") {
		t.Errorf("Y: should execute when conditionResult is true: %q", result)
	}

	// N: should not execute when condition is true
	result, err = e.Execute("N:T:Condition is false")
	if err != nil {
		t.Fatalf("Execute(N:) error = %v", err)
	}
	if strings.Contains(result, "NO") {
		t.Errorf("N: should return empty when conditionResult is true: %q", result)
	}

	// Set condition to false
	e.conditionResult = false

	// Y: should not execute when condition is false
	result, err = e.Execute("Y:T:Should not appear")
	if err != nil {
		t.Fatalf("Execute(Y:) error = %v", err)
	}
	if result != "" {
		t.Errorf("Y: should return empty when conditionResult is false: %q", result)
	}

	// N: should execute when condition is false
	result, err = e.Execute("N:T:Now it works")
	if err != nil {
		t.Fatalf("Execute(N:) error = %v", err)
	}
	if !strings.Contains(result, "NO") {
		t.Errorf("N: should execute when conditionResult is false: %q", result)
	}
}

// TestExecutor_Match tests M: (match pattern) command
func TestExecutor_Match(t *testing.T) {
	e := New()
	e.lastInput = "quick"

	// Match exact pattern (returns empty string)
	_, err := e.Execute("M:quick")
	if err != nil {
		t.Fatalf("Execute(M:quick) error = %v", err)
	}
	if !e.matchFlag {
		t.Error("M:quick should set matchFlag to true for exact match")
	}

	// Match non-existing pattern
	_, err = e.Execute("M:elephant")
	if err != nil {
		t.Fatalf("Execute(M:elephant) error = %v", err)
	}
	if e.matchFlag {
		t.Error("M:elephant should set matchFlag to false")
	}
}

// TestExecutor_Remark tests R: (remark/comment) command
func TestExecutor_Remark(t *testing.T) {
	e := New()
	result, err := e.Execute("R:This is a comment")
	if err != nil {
		t.Fatalf("Execute(R:) error = %v", err)
	}
	// Remark should not produce output
	if strings.Contains(result, "comment") {
		t.Errorf("R: should not produce output: %q", result)
	}
}

// TestExecutor_End tests E: (end program) command
func TestExecutor_End(t *testing.T) {
	e := New()
	result, err := e.Execute("E:")
	if err != nil {
		t.Fatalf("Execute(E:) error = %v", err)
	}
	if !strings.Contains(result, "✅") {
		t.Errorf("E: should contain success emoji: %q", result)
	}
}

// TestExecutor_InvalidCommand tests error handling for invalid commands
func TestExecutor_InvalidCommand(t *testing.T) {
	e := New()

	// Command without colon
	result, err := e.Execute("TFOOBAR")
	if err != nil {
		t.Fatalf("Execute() should not return error: %v", err)
	}
	// Should produce no output or error for malformed command
	_ = result

	// Unknown command type
	result, err = e.Execute("Z:Unknown")
	if err != nil {
		t.Fatalf("Execute() should not return error: %v", err)
	}
	if !strings.Contains(result, "❌") {
		t.Errorf("Unknown command should return error with ❌: %q", result)
	}
}

// TestExecutor_RunProgram tests multi-line PILOT program execution
func TestExecutor_RunProgram(t *testing.T) {
	e := New()
	program := `
T:Welcome to PILOT
U:NAME=Alice
T:Hello, *NAME*
E:
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "Welcome to PILOT") {
		t.Errorf("Program output missing 'Welcome to PILOT': %q", result)
	}
	if !strings.Contains(result, "Alice") {
		t.Errorf("Program output missing 'Alice': %q", result)
	}
}

// TestExecutor_JumpLabel tests J: (jump) and L: (label) commands
func TestExecutor_JumpLabel(t *testing.T) {
	e := New()
	program := `
T:Start
J:SKIP
T:This should be skipped
L:SKIP
T:After jump
E:
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "Start") {
		t.Errorf("Expected 'Start' in output: %q", result)
	}
	if strings.Contains(result, "skipped") {
		t.Errorf("Should not contain 'skipped' (jumped over): %q", result)
	}
	if !strings.Contains(result, "After jump") {
		t.Errorf("Expected 'After jump' in output: %q", result)
	}
}

// TestExecutor_ConditionalFlow tests conditional execution with C:, Y:, N:
func TestExecutor_ConditionalFlow(t *testing.T) {
	e := New()
	program := `
U:X=5
U:Y=10
C:X<Y
Y:
T:X is less than Y
N:
T:X is not less than Y
E:
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "X is less than Y") {
		t.Errorf("Expected 'X is less than Y': %q", result)
	}
	if strings.Contains(result, "not less") {
		t.Errorf("Should not contain 'not less': %q", result)
	}
}

// TestExecutor_VariableSubstitution tests variable substitution in text
func TestExecutor_VariableSubstitution(t *testing.T) {
	e := New()
	e.variables["USER"] = "Bob"
	e.variables["COUNT"] = "42"

	// PILOT uses *VAR* syntax for interpolation
	result, err := e.Execute("T:User *USER* has *COUNT* points")
	if err != nil {
		t.Fatalf("Execute() error = %v", err)
	}

	if !strings.Contains(result, "Bob") {
		t.Errorf("Expected 'Bob' in output: %q", result)
	}
	if !strings.Contains(result, "42") {
		t.Errorf("Expected '42' in output: %q", result)
	}
}

// TestExecutor_ComplexProgram tests a more complex multi-command program
func TestExecutor_ComplexProgram(t *testing.T) {
	e := New()
	program := `
R:This is a test program
U:SCORE=100
U:GRADE=A
T:Your grade is *GRADE*
T:Score is *SCORE*
E:
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "Your grade is A") {
		t.Errorf("Expected 'Your grade is A': %q", result)
	}
	if !strings.Contains(result, "Score is 100") {
		t.Errorf("Expected 'Score is 100': %q", result)
	}
	if strings.Contains(result, "test program") {
		t.Errorf("Comments should not appear in output: %q", result)
	}
}

// --- New tests to improve coverage for expressions, conditions, and edge cases ---

// TestExecutor_EvaluateExpression_Arithmetic covers +, -, *, / and divide-by-zero behavior
func TestExecutor_EvaluateExpression_Arithmetic(t *testing.T) {
	e := New()

	// Addition
	if _, err := e.Execute("U:A=2+3"); err != nil {
		t.Fatalf("U:A=2+3 error: %v", err)
	}
	if e.variables["A"] != "5" {
		t.Errorf("A expected 5, got %q", e.variables["A"])
	}

	// Subtraction
	if _, err := e.Execute("U:B=10-4"); err != nil {
		t.Fatalf("U:B=10-4 error: %v", err)
	}
	if e.variables["B"] != "6" {
		t.Errorf("B expected 6, got %q", e.variables["B"])
	}

	// Multiplication
	if _, err := e.Execute("U:C=3*7"); err != nil {
		t.Fatalf("U:C=3*7 error: %v", err)
	}
	if e.variables["C"] != "21" {
		t.Errorf("C expected 21, got %q", e.variables["C"])
	}

	// Division
	if _, err := e.Execute("U:D=8/2"); err != nil {
		t.Fatalf("U:D=8/2 error: %v", err)
	}
	if e.variables["D"] != "4" {
		t.Errorf("D expected 4, got %q", e.variables["D"])
	}

	// Divide by zero: current behavior keeps literal expression string
	if _, err := e.Execute("U:E=5/0"); err != nil {
		t.Fatalf("U:E=5/0 error: %v", err)
	}
	if e.variables["E"] != "5/0" {
		t.Errorf("E expected literal '5/0' on divide-by-zero, got %q", e.variables["E"])
	}
}

// TestExecutor_EvaluateExpression_VariablesAndStrings covers variable reference and string literal
func TestExecutor_EvaluateExpression_VariablesAndStrings(t *testing.T) {
	e := New()
	e.variables["NAME"] = "Zoe"

	// Variable reference
	if _, err := e.Execute("U:PERSON=NAME"); err != nil {
		t.Fatalf("U:PERSON=NAME error: %v", err)
	}
	if e.variables["PERSON"] != "Zoe" {
		t.Errorf("PERSON expected Zoe, got %q", e.variables["PERSON"])
	}

	// String literal (quotes trimmed)
	if _, err := e.Execute("U:GREET=\"Hi\""); err != nil {
		t.Fatalf("U:GREET=\"Hi\" error: %v", err)
	}
	if e.variables["GREET"] != "Hi" {
		t.Errorf("GREET expected Hi, got %q", e.variables["GREET"])
	}
}

// TestExecutor_EvaluateCondition_AllOperators covers numeric and string comparisons
func TestExecutor_EvaluateCondition_AllOperators(t *testing.T) {
	e := New()

	// Numeric true cases
	for _, cond := range []string{"2=2", "3>2", "2<3", "3>=3", "2<=3", "2<>3"} {
		if _, err := e.Execute("C:" + cond); err != nil {
			t.Fatalf("C:%s error: %v", cond, err)
		}
		if !e.conditionResult {
			t.Errorf("Condition should be true: %s", cond)
		}
	}

	// Numeric false cases
	for _, cond := range []string{"2=3", "2>3", "3<2", "2>=3", "3<=2", "2<>2"} {
		if _, err := e.Execute("C:" + cond); err != nil {
			t.Fatalf("C:%s error: %v", cond, err)
		}
		if e.conditionResult {
			t.Errorf("Condition should be false: %s", cond)
		}
	}

	// String equality and inequality
	if _, err := e.Execute("C:\"A\"=\"A\""); err != nil {
		t.Fatalf("C:\"A\"=\"A\" error: %v", err)
	}
	if !e.conditionResult {
		t.Error("String equality should be true")
	}
	if _, err := e.Execute("C:\"A\"<>\"B\""); err != nil {
		t.Fatalf("C:\"A\"<>\"B\" error: %v", err)
	}
	if !e.conditionResult {
		t.Error("String inequality should be true")
	}
}

// TestExecutor_Use_PrintVariable covers printing variable without assignment via U:
func TestExecutor_Use_PrintVariable(t *testing.T) {
	e := New()
	e.variables["SCORE"] = "42"
	out, err := e.Execute("U:SCORE")
	if err != nil {
		t.Fatalf("U:SCORE error: %v", err)
	}
	if !strings.Contains(out, "SCORE = 42") {
		t.Errorf("Expected 'SCORE = 42' in output, got %q", out)
	}
}

// TestExecutor_YN_WithMatchFlag validates Y and N behavior when matchFlag is true
func TestExecutor_YN_WithMatchFlag(t *testing.T) {
	e := New()
	e.matchFlag = true

	out, err := e.Execute("Y:T:matched")
	if err != nil {
		t.Fatalf("Y: error: %v", err)
	}
	if !strings.Contains(out, "YES") {
		t.Errorf("Y: expected YES when matchFlag=true, got %q", out)
	}

	out, err = e.Execute("N:T:not matched")
	if err != nil {
		t.Fatalf("N: error: %v", err)
	}
	if out != "" {
		t.Errorf("N: expected empty when matchFlag=true, got %q", out)
	}
}

// TestExecutor_JumpMissingLabel tests Execute path for missing label jump
func TestExecutor_JumpMissingLabel(t *testing.T) {
	e := New()
	out, err := e.Execute("J:NO_LABEL")
	if err != nil {
		t.Fatalf("J:NO_LABEL error: %v", err)
	}
	if !strings.Contains(out, "Label 'NO_LABEL' not found") {
		t.Errorf("Expected missing label message, got %q", out)
	}
}

// TestExecutor_RunProgram_InfiniteLoop triggers the maxSteps guard
func TestExecutor_RunProgram_InfiniteLoop(t *testing.T) {
	e := New()
	program := `
L:LOOP
J:LOOP
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "too many steps") {
		t.Errorf("Expected too many steps guard, got %q", out)
	}
}

// TestExecutor_RunProgram_MissingLabel checks J: missing label within program
func TestExecutor_RunProgram_MissingLabel(t *testing.T) {
	e := New()
	program := `
J:UNKNOWN
E:
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "Label 'UNKNOWN' not found") {
		t.Errorf("Expected missing label error in program, got %q", out)
	}
}

// TestExecutor_RunProgram_Y_Skip verifies Y: skips the next line when the condition is false
func TestExecutor_RunProgram_Y_Skip(t *testing.T) {
	e := New()
	program := `
U:X=1
C:X>5
Y:
T:SHOULD_NOT_PRINT
T:SHOULD_PRINT
E:
`
	out := e.RunProgram(program)
	if strings.Contains(out, "SHOULD_NOT_PRINT") {
		t.Errorf("Y: should skip next when condition false; got %q", out)
	}
	if !strings.Contains(out, "SHOULD_PRINT") {
		t.Errorf("Expected 'SHOULD_PRINT' in output; got %q", out)
	}
}

// TestExecutor_RunProgram_N_Skip verifies N: skips the next line when the condition is true
func TestExecutor_RunProgram_N_Skip(t *testing.T) {
	e := New()
	program := `
U:X=10
C:X>=10
N:
T:SHOULD_NOT_PRINT
T:SHOULD_PRINT
E:
`
	out := e.RunProgram(program)
	if strings.Contains(out, "SHOULD_NOT_PRINT") {
		t.Errorf("N: should skip next when condition true; got %q", out)
	}
	if !strings.Contains(out, "SHOULD_PRINT") {
		t.Errorf("Expected 'SHOULD_PRINT' in output; got %q", out)
	}
}

// TestExecutor_RunProgram_Match_Y_OR tests Y: executing based on matchFlag via M:
func TestExecutor_RunProgram_Match_Y_OR(t *testing.T) {
	e := New()
	program := `
M:hello
Y:
T:NO_MATCH
M:*
Y:
T:MATCHED
E:
`
	out := e.RunProgram(program)
	if strings.Contains(out, "NO_MATCH") {
		t.Errorf("First Y should not execute when M:hello doesn't match empty input; got %q", out)
	}
	if !strings.Contains(out, "MATCHED") {
		t.Errorf("Second Y should execute when M:* matches; got %q", out)
	}
}

// TestExecutor_Interpolate_MissingVar ensures unknown variables remain as *VAR*
func TestExecutor_Interpolate_MissingVar(t *testing.T) {
	e := New()
	out, err := e.Execute("T:*MISSING*")
	if err != nil {
		t.Fatalf("T:*MISSING* error: %v", err)
	}
	if !strings.Contains(out, "*MISSING*") {
		t.Errorf("Expected literal *MISSING* preserved, got %q", out)
	}
}

// TestExecutor_Execute_EmptyWhitespace ensures empty/whitespace commands return empty output
func TestExecutor_Execute_EmptyWhitespace(t *testing.T) {
	e := New()
	out, err := e.Execute("")
	if err != nil {
		t.Fatalf("empty command error: %v", err)
	}
	if out != "" {
		t.Errorf("empty command should return empty output, got %q", out)
	}
	out, err = e.Execute("   \t  ")
	if err != nil {
		t.Fatalf("whitespace command error: %v", err)
	}
	if out != "" {
		t.Errorf("whitespace command should return empty output, got %q", out)
	}
}

// TestExecutor_RunProgram_SkipUnknownLine ensures bare lines without colon are skipped
func TestExecutor_RunProgram_SkipUnknownLine(t *testing.T) {
	e := New()
	program := `
FOO
T:AFTER
E:
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "AFTER") {
		t.Errorf("Expected AFTER in output; got %q", out)
	}
}
