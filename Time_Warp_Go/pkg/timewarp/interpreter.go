package timewarp

import (
	"fmt"
	"strings"

	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp/executors/basic"
	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp/executors/logo"
	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp/executors/pilot"
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

// Execute parses a single command and returns the output string.
// Errors are embedded in the returned string with the ❌ prefix.
func (i *Interpreter) Execute(command string) string {
	cmd := strings.TrimSpace(command)
	if cmd == "" {
		return ""
	}

	upper := strings.ToUpper(cmd)
	switch {
	// Basic-like commands
	case strings.HasPrefix(upper, "PRINT ") || strings.HasPrefix(upper, "ECHO "):
		out, err := i.basicExec.Execute(cmd)
		if err != nil {
			return fmt.Sprintf("❌ %v\n", err)
		}
		return out

	// Logo-like commands
	case strings.HasPrefix(upper, "FORWARD ") || strings.HasPrefix(upper, "FD ") || strings.HasPrefix(upper, "RIGHT ") || strings.HasPrefix(upper, "LEFT "):
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
		return "ℹ️  Commands: PRINT/ECHO <text>, FORWARD/FD <n>, RIGHT/LEFT <deg>, T:/A: <text>\n"
	}
	if upper == "RUN" {
		return "🚀 Running (stub)\n"
	}
	if strings.HasPrefix(upper, "THEME ") {
		return "🎨 Theme change (stub)\n"
	}

	return "❌ Unknown command — try HELP\n"
}
