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
