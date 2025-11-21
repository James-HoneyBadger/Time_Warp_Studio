package pilot

import (
	"strings"
	"testing"
)

// TestExecutor_Execute_JumpSuccess tests J: successfully finding label and setting currentLine
func TestExecutor_Execute_JumpSuccess(t *testing.T) {
	e := New()
	e.labels["TARGET"] = 42
	out, err := e.Execute("J:TARGET")
	if err != nil {
		t.Fatalf("J:TARGET error: %v", err)
	}
	if !strings.Contains(out, "JUMP to TARGET") {
		t.Errorf("Expected JUMP message, got %q", out)
	}
	if e.currentLine != 42 {
		t.Errorf("currentLine should be set to 42, got %d", e.currentLine)
	}
}

// TestExecutor_Execute_LabelDefine tests L: defining a label in Execute
func TestExecutor_Execute_LabelDefine(t *testing.T) {
	e := New()
	e.currentLine = 10
	out, err := e.Execute("L:MYLABEL")
	if err != nil {
		t.Fatalf("L:MYLABEL error: %v", err)
	}
	if out != "" {
		t.Errorf("L: should return empty output, got %q", out)
	}
	if e.labels["MYLABEL"] != 10 {
		t.Errorf("Label MYLABEL should map to 10, got %d", e.labels["MYLABEL"])
	}
}

// TestExecutor_EvaluateCondition_NoOperator ensures no-operator conditions return false
func TestExecutor_EvaluateCondition_NoOperator(t *testing.T) {
	e := New()
	_, err := e.Execute("C:JUSTTEXT")
	if err != nil {
		t.Fatalf("C:JUSTTEXT error: %v", err)
	}
	if e.conditionResult {
		t.Error("Condition without operator should be false")
	}
}

// TestExecutor_RunProgram_Accept tests A: in RunProgram clearing variables
func TestExecutor_RunProgram_Accept(t *testing.T) {
	e := New()
	program := `
A:INPUT_VAR
T:Accepted
E:
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "Accepted") {
		t.Errorf("Expected 'Accepted' in output, got %q", out)
	}
	if e.variables["INPUT_VAR"] != "" {
		t.Errorf("A: should initialize INPUT_VAR to empty, got %q", e.variables["INPUT_VAR"])
	}
}

// TestExecutor_RunProgram_LabelBeforeJump tests label defined before jump target
func TestExecutor_RunProgram_LabelBeforeJump(t *testing.T) {
	e := New()
	program := `
L:START
T:At start
J:END
T:Skipped
L:END
T:At end
E:
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "At start") {
		t.Errorf("Expected 'At start', got %q", out)
	}
	if strings.Contains(out, "Skipped") {
		t.Errorf("Should not contain 'Skipped', got %q", out)
	}
	if !strings.Contains(out, "At end") {
		t.Errorf("Expected 'At end', got %q", out)
	}
}

// TestExecutor_RunProgram_EmptyContent ensures programs with only whitespace/empty lines work
func TestExecutor_RunProgram_EmptyContent(t *testing.T) {
	e := New()
	program := `


T:OK
E:
`
	out := e.RunProgram(program)
	if !strings.Contains(out, "OK") {
		t.Errorf("Expected 'OK' in output, got %q", out)
	}
}

// TestExecutor_Execute_ShortCommand tests commands with len < 2
func TestExecutor_Execute_ShortCommand(t *testing.T) {
	e := New()
	out, err := e.Execute("T")
	if err != nil {
		t.Fatalf("Short command error: %v", err)
	}
	if out != "" {
		t.Errorf("Short command should return empty, got %q", out)
	}
}

// TestExecutor_EvaluateCondition_StringComparison tests string inequality operators
func TestExecutor_EvaluateCondition_StringComparison(t *testing.T) {
	e := New()

	// String equality false
	_, err := e.Execute("C:\"A\"=\"B\"")
	if err != nil {
		t.Fatalf("String comparison error: %v", err)
	}
	if e.conditionResult {
		t.Error("String equality A=B should be false")
	}

	// String inequality false
	_, err = e.Execute("C:\"X\"<>\"X\"")
	if err != nil {
		t.Fatalf("String comparison error: %v", err)
	}
	if e.conditionResult {
		t.Error("String inequality X<>X should be false")
	}
}
