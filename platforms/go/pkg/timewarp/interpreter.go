// SPDX-License-Identifier: MIT
package timewarp

import (
	"fmt"
	"strings"

	"github.com/James-HoneyBadger/Time_Warp/platforms/go/pkg/timewarp/executors/basic"
	"github.com/James-HoneyBadger/Time_Warp/platforms/go/pkg/timewarp/executors/logo"
	"github.com/James-HoneyBadger/Time_Warp/platforms/go/pkg/timewarp/executors/pilot"
)

// Interpreter is a tiny, educational stub for the Go version of Time Warp.
// It routes simple commands to language executors and returns text output
// with emoji prefixes matching the project conventions.
//
// Emoji prefixes:
// - ❌ = Errors/exceptions
// - ✅ = Success confirmations
// - ℹ️ = Informational messages
// - 🎨 = Theme/UI changes
// - 🚀 = Execution/run events
// - 🐢 = Turtle graphics actions
// - 📝 = Input prompts
//
// NOTE: This is a minimal, non-UI console version to start exploring a Go port.
// It intentionally keeps state outside of any UI and returns text only.

type Interpreter struct {
	basicExec *basic.Executor
	logoExec  *logo.Executor
	pilotExec *pilot.Executor
}

// NewInterpreter constructs a new Interpreter instance.
func NewInterpreter() *Interpreter {
	return &Interpreter{
		basicExec: basic.New(),
		logoExec:  logo.New(),
		pilotExec: pilot.New(),
	}
}

// Logo returns the Logo executor for accessing structured turtle events/state.
// This preserves the text-output contract while allowing UIs to consume events.
func (i *Interpreter) Logo() *logo.Executor { return i.logoExec }

// Execute parses a single command and returns the output string.
// Errors are embedded in the returned string with the ❌ prefix.
func (i *Interpreter) Execute(command string) string {
	cmd := strings.TrimSpace(command)
	if cmd == "" {
		return ""
	}

	// If multi-line input, assume BASIC program and execute via RunProgram
	if strings.Contains(cmd, "\n") || strings.Contains(cmd, "\r") {
		// Treat as BASIC multi-line program; append END if missing to terminate
		program := cmd
		upProg := strings.ToUpper(strings.TrimSpace(program))
		if !strings.Contains(upProg, "END") {
			program += "\nEND"
		}
		out := i.basicExec.RunProgram(program)
		if out == "" {
			return "✅ Program finished\n"
		}
		return out
	}

	upper := strings.ToUpper(cmd)
	switch {
	// Basic-like commands
	case strings.HasPrefix(upper, "PRINT ") || strings.HasPrefix(upper, "ECHO ") ||
		strings.HasPrefix(upper, "LET ") ||
		(strings.Contains(cmd, "=") && !strings.HasPrefix(upper, "IF ") && !strings.HasPrefix(upper, "FOR ")):
		out, err := i.basicExec.Execute(cmd)
		if err != nil {
			return fmt.Sprintf("❌ %v\n", err)
		}
		return out

	// Logo-like commands
	case strings.HasPrefix(upper, "FORWARD ") || strings.HasPrefix(upper, "FD ") ||
		strings.HasPrefix(upper, "BACK ") || strings.HasPrefix(upper, "BK ") || strings.HasPrefix(upper, "BACKWARD ") ||
		strings.HasPrefix(upper, "RIGHT ") || strings.HasPrefix(upper, "RT ") ||
		strings.HasPrefix(upper, "LEFT ") || strings.HasPrefix(upper, "LT ") ||
		upper == "HOME" || upper == "CLEARSCREEN" || upper == "CS" || upper == "CLEAR" ||
		strings.HasPrefix(upper, "SETXY ") || strings.HasPrefix(upper, "SETHEADING ") || strings.HasPrefix(upper, "SETH ") ||
		strings.HasPrefix(upper, "SETCOLOR ") || strings.HasPrefix(upper, "SETPENCOLOR ") || strings.HasPrefix(upper, "SETPC ") ||
		strings.HasPrefix(upper, "PENWIDTH ") || strings.HasPrefix(upper, "SETPENWIDTH ") || strings.HasPrefix(upper, "SETPW ") || strings.HasPrefix(upper, "SETPENSIZE ") ||
		upper == "PENUP" || upper == "PU" || upper == "PENDOWN" || upper == "PD" ||
		upper == "HIDETURTLE" || upper == "HT" || upper == "SHOWTURTLE" || upper == "ST":
		out, err := i.logoExec.Execute(cmd)
		if err != nil {
			return fmt.Sprintf("❌ %v\n", err)
		}
		return out

	// PILOT-like (prefixes like T: or A: are commonly used in PILOT variants)
	case strings.HasPrefix(upper, "T:") || strings.HasPrefix(upper, "A:"):
		out, err := i.pilotExec.Execute(cmd)
		if err != nil {
			return fmt.Sprintf("❌ %v\n", err)
		}
		return out
	}

	// Misc convenience commands
	if upper == "HELP" {
		return "ℹ️  Commands: PRINT/ECHO <text>, LET x = 1, FOR/NEXT, IF/THEN, GOTO, T:/A: <text>, LOGO: FD/BK/LT/RT/HOME/PU/PD/CS/SETXY/SETH/SETCOLOR/PENWIDTH\n"
	}
	if upper == "RUN" {
		return "🚀 Running (stub)\n"
	}
	if strings.HasPrefix(upper, "THEME ") {
		return "🎨 Theme change (stub)\n"
	}

	return "❌ Unknown command — try HELP\n"
}
