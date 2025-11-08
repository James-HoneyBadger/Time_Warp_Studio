package basic

import (
	"strings"
	"testing"
)

// TestExecutor_Print tests PRINT command with various inputs
func TestExecutor_Print(t *testing.T) {
	tests := []struct {
		name     string
		command  string
		expected string
	}{
		{
			name:     "print simple text",
			command:  `PRINT "Hello, World!"`,
			expected: "Hello, World!\n",
		},
		{
			name:     "print number",
			command:  "PRINT 42",
			expected: "42\n",
		},
		{
			name:     "print expression",
			command:  "PRINT 2 + 3 * 4",
			expected: "14\n",
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
				t.Errorf("Execute() = %q, want %q", result, tt.expected)
			}
		})
	}
}

// TestExecutor_LetAndVariables tests LET command and variable assignment
func TestExecutor_LetAndVariables(t *testing.T) {
	e := New()

	// Test LET assignment (returns empty string on success)
	_, err := e.Execute("LET X = 10")
	if err != nil {
		t.Fatalf("Execute(LET) error = %v", err)
	}

	// Verify variable was set
	if val, ok := e.variables["X"]; !ok || val != 10 {
		t.Errorf("Variable X = %v, want 10", val)
	}

	// Test printing variable
	result, err := e.Execute("PRINT X")
	if err != nil {
		t.Fatalf("Execute(PRINT X) error = %v", err)
	}
	if !strings.Contains(result, "10") {
		t.Errorf("PRINT X = %q, want '10'", result)
	}

	// Test assignment without LET
	_, err = e.Execute("Y = 20")
	if err != nil {
		t.Fatalf("Execute(Y=20) error = %v", err)
	}
	if val, ok := e.variables["Y"]; !ok || val != 20 {
		t.Errorf("Variable Y = %v, want 20", val)
	}
}

// TestExecutor_Expressions tests mathematical expressions
func TestExecutor_Expressions(t *testing.T) {
	tests := []struct {
		name     string
		setup    []string
		command  string
		expected string
	}{
		{
			name:     "addition",
			command:  "PRINT 5 + 3",
			expected: "8",
		},
		{
			name:     "multiplication",
			command:  "PRINT 4 * 7",
			expected: "28",
		},
		{
			name:     "order of operations",
			command:  "PRINT 2 + 3 * 4",
			expected: "14",
		},
		{
			name:     "variable in expression",
			setup:    []string{"X = 5"},
			command:  "PRINT X * 2",
			expected: "10",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := New()
			for _, cmd := range tt.setup {
				if _, err := e.Execute(cmd); err != nil {
					t.Fatalf("Setup command %q failed: %v", cmd, err)
				}
			}
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

// TestExecutor_ForNext tests FOR/NEXT loop functionality
func TestExecutor_ForNext(t *testing.T) {
	e := New()
	program := `
10 FOR I = 1 TO 5
20 PRINT I
30 NEXT I
40 END
`
	result := e.RunProgram(program)

	// Should print 1, 2, 3, 4, 5
	for i := 1; i <= 5; i++ {
		expected := string(rune('0' + i))
		if !strings.Contains(result, expected) {
			t.Errorf("FOR/NEXT output missing %s: %q", expected, result)
		}
	}
}

// TestExecutor_ForNextStep tests FOR/NEXT with STEP
func TestExecutor_ForNextStep(t *testing.T) {
	e := New()
	program := `
10 FOR I = 0 TO 10 STEP 2
20 PRINT I
30 NEXT I
`
	result := e.RunProgram(program)

	// Should print 0, 2, 4, 6, 8, 10
	expected := []string{"0", "2", "4", "6", "8", "10"}
	for _, val := range expected {
		if !strings.Contains(result, val) {
			t.Errorf("FOR/NEXT STEP output missing %s: %q", val, result)
		}
	}
}

// TestExecutor_GosubReturn tests GOSUB/RETURN subroutine functionality
func TestExecutor_GosubReturn(t *testing.T) {
	e := New()
	program := `
10 PRINT "START"
20 GOSUB 100
30 PRINT "END"
40 END
100 PRINT "SUBROUTINE"
110 RETURN
`
	result := e.RunProgram(program)

	// Check correct order: START, SUBROUTINE, END
	startIdx := strings.Index(result, "START")
	subIdx := strings.Index(result, "SUBROUTINE")
	endIdx := strings.Index(result, "END")

	if startIdx == -1 || subIdx == -1 || endIdx == -1 {
		t.Errorf("GOSUB/RETURN missing expected output: %q", result)
	}
	if !(startIdx < subIdx && subIdx < endIdx) {
		t.Errorf("GOSUB/RETURN incorrect order: START(%d), SUB(%d), END(%d)", startIdx, subIdx, endIdx)
	}
}

// TestExecutor_IfThen tests IF/THEN conditional execution
func TestExecutor_IfThen(t *testing.T) {
	e := New()
	// Test a simple program where IF evaluates correctly
	program := `
10 LET X = 5
20 LET Y = 3
30 IF X < Y THEN 60
40 PRINT "X >= Y"
50 END
60 PRINT "X < Y"
70 END
`
	result := e.RunProgram(program)

	// X=5, Y=3, so X<Y is false, should print "X >= Y"
	if !strings.Contains(result, "X >= Y") {
		t.Errorf("IF/THEN: expected 'X >= Y' in output: %q", result)
	}
	if strings.Contains(result, "X < Y") {
		t.Errorf("IF/THEN: unexpected 'X < Y' in output: %q", result)
	}
}

// TestExecutor_Goto tests GOTO command
func TestExecutor_Goto(t *testing.T) {
	e := New()
	program := `
10 PRINT "A"
20 GOTO 40
30 PRINT "B"
40 PRINT "C"
50 END
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "A") || !strings.Contains(result, "C") {
		t.Errorf("GOTO: expected 'A' and 'C': %q", result)
	}
	if strings.Contains(result, "B") {
		t.Errorf("GOTO: unexpected 'B' (should be skipped): %q", result)
	}
}

// TestExecutor_Rem tests REM comment command
func TestExecutor_Rem(t *testing.T) {
	e := New()
	program := `
10 REM This is a comment
20 PRINT "Hello"
30 REM Another comment
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "Hello") {
		t.Errorf("REM: expected 'Hello': %q", result)
	}
	if strings.Contains(result, "comment") {
		t.Errorf("REM: comments should not appear in output: %q", result)
	}
}

// TestExecutor_Cls tests CLS command
func TestExecutor_Cls(t *testing.T) {
	e := New()
	result, err := e.Execute("CLS")
	if err != nil {
		t.Fatalf("Execute(CLS) error = %v", err)
	}
	if !strings.Contains(result, "🎨") {
		t.Errorf("CLS should contain screen clear emoji: %q", result)
	}
}

// TestExecutor_GraphicsCommands tests LINE and CIRCLE commands
func TestExecutor_GraphicsCommands(t *testing.T) {
	e := New()

	// Test LINE (uses 📏 emoji, not 🎨)
	result, err := e.Execute("LINE 10,20,30,40")
	if err != nil {
		t.Fatalf("Execute(LINE) error = %v", err)
	}
	if !strings.Contains(result, "📏") {
		t.Errorf("LINE should contain line emoji: %q", result)
	}

	// Test CIRCLE (uses ⭕ emoji, not 🎨)
	result, err = e.Execute("CIRCLE 50,50,25")
	if err != nil {
		t.Fatalf("Execute(CIRCLE) error = %v", err)
	}
	if !strings.Contains(result, "⭕") {
		t.Errorf("CIRCLE should contain circle emoji: %q", result)
	}
}

// TestExecutor_Locate tests LOCATE command
func TestExecutor_Locate(t *testing.T) {
	e := New()
	result, err := e.Execute("LOCATE 10,20")
	if err != nil {
		t.Fatalf("Execute(LOCATE) error = %v", err)
	}
	if !strings.Contains(result, "📍") {
		t.Errorf("LOCATE should contain location emoji: %q", result)
	}
}

// TestExecutor_End tests END command
func TestExecutor_End(t *testing.T) {
	e := New()
	program := `
10 PRINT "Before END"
20 END
30 PRINT "After END"
`
	result := e.RunProgram(program)

	if !strings.Contains(result, "Before END") {
		t.Errorf("END: expected 'Before END': %q", result)
	}
	if strings.Contains(result, "After END") {
		t.Errorf("END: should not execute after END: %q", result)
	}
}

// TestExecutor_UnknownCommand tests error handling for unknown commands
func TestExecutor_UnknownCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("FOOBAR 123")
	if err != nil {
		t.Fatalf("Execute() should not return error for unknown command: %v", err)
	}
	if !strings.Contains(result, "❌") {
		t.Errorf("Unknown command should return error message with ❌: %q", result)
	}
}

// TestExecutor_NestedForLoops tests nested FOR/NEXT loops
func TestExecutor_NestedForLoops(t *testing.T) {
	e := New()
	program := `
10 FOR I = 1 TO 2
20 FOR J = 1 TO 2
30 PRINT I * 10 + J
40 NEXT J
50 NEXT I
`
	result := e.RunProgram(program)

	// Should print 11, 12, 21, 22
	expected := []string{"11", "12", "21", "22"}
	for _, val := range expected {
		if !strings.Contains(result, val) {
			t.Errorf("Nested FOR loops missing %s: %q", val, result)
		}
	}
}
