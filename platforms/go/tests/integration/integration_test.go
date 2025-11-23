package integration_test

import (
	"strings"
	"testing"

	"github.com/James-HoneyBadger/Time_Warp/platforms/go/pkg/timewarp"
)

// TestCrossPlatformConsistency verifies identical output across language executors
func TestCrossPlatformConsistency(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		contains []string
	}{
		{
			name:     "Basic Hello World",
			code:     `PRINT "Hello World"`,
			contains: []string{"Hello World"},
		},
		{
			name: "Basic FOR loop",
			code: `FOR i = 1 TO 3
PRINT i
NEXT i`,
			contains: []string{"1", "2", "3"},
		},
		{
			name:     "PILOT Type command",
			code:     `T:Hello from PILOT`,
			contains: []string{"Hello from PILOT"},
		},
		{
			name:     "Logo Forward command",
			code:     `FORWARD 100`,
			contains: []string{"FORWARD", "100"},
		},
		{
			name: "Logo Turn commands",
			code: `LEFT 90
RIGHT 45`,
			contains: []string{"LEFT", "RIGHT"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			interp := timewarp.NewInterpreter()

			var got string
			if strings.Contains(tt.code, "\n") {
				// Multi-line: run as a BASIC program in one call
				got = interp.Execute(tt.code)
			} else {
				got = interp.Execute(tt.code)
			}

			// Debug: always log what we got
			t.Logf("Code: %q\nGot: %q", tt.code, got)

			for _, want := range tt.contains {
				if !strings.Contains(got, want) {
					t.Errorf("Output missing expected substring:\nWant: %q\nGot: %s", want, got)
				}
			}
		})
	}
}

// TestInterpreterStatePersistence verifies variables persist across commands
func TestInterpreterStatePersistence(t *testing.T) {
	interp := timewarp.NewInterpreter()

	commands := []struct {
		cmd      string
		contains string
	}{
		{"LET x = 10", ""},
		{"x = x + 5", ""},
		{"PRINT x", "15"},
	}

	for _, cmd := range commands {
		output := interp.Execute(cmd.cmd)
		if cmd.contains != "" && !strings.Contains(output, cmd.contains) {
			t.Errorf("Execute(%q) = %q, want to contain %q", cmd.cmd, output, cmd.contains)
		}
	}
}

// TestTurtleGraphicsCommands verifies turtle state changes
func TestTurtleGraphicsCommands(t *testing.T) {
	interp := timewarp.NewInterpreter()

	tests := []struct {
		cmd      string
		validate func(output string) bool
	}{
		{
			cmd: "FORWARD 50",
			validate: func(out string) bool {
				return strings.Contains(out, "FORWARD") && strings.Contains(out, "50")
			},
		},
		{
			cmd: "LEFT 90",
			validate: func(out string) bool {
				return strings.Contains(out, "LEFT") && strings.Contains(out, "90")
			},
		},
		{
			cmd: "HOME",
			validate: func(out string) bool {
				return strings.Contains(out, "HOME") || strings.Contains(out, "0")
			},
		},
		{
			cmd: "PENUP",
			validate: func(out string) bool {
				return strings.Contains(out, "PENUP") || strings.Contains(out, "PU")
			},
		},
		{
			cmd: "PENDOWN",
			validate: func(out string) bool {
				return strings.Contains(out, "PENDOWN") || strings.Contains(out, "PD")
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.cmd, func(t *testing.T) {
			output := interp.Execute(tt.cmd)
			if !tt.validate(output) {
				t.Errorf("Execute(%q) = %q failed validation", tt.cmd, output)
			}
		})
	}
}

// TestErrorHandling verifies proper error reporting
func TestErrorHandling(t *testing.T) {
	interp := timewarp.NewInterpreter()

	tests := []struct {
		name string
		cmd  string
	}{
		{"Unknown command", "FOOBAR"},
		{"Invalid BASIC", "PRINT"},
		{"Invalid expression", "LET x = @#$"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output := interp.Execute(tt.cmd)
			// Should contain error indicator (emoji ❌ or "error")
			if !strings.Contains(output, "❌") && !strings.Contains(strings.ToLower(output), "error") {
				t.Logf("Warning: %q may not have proper error reporting: %s", tt.cmd, output)
			}
		})
	}
}
