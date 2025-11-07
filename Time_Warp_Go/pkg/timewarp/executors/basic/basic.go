package basic

import (
	"strings"
)

// Executor handles very small subset of BASIC-like commands.
// Supported:
//   - PRINT <text>
//   - ECHO <text>

type Executor struct{}

func New() *Executor { return &Executor{} }

func (e *Executor) Execute(command string) (string, error) {
	c := strings.TrimSpace(command)
	up := strings.ToUpper(c)
	if strings.HasPrefix(up, "PRINT ") {
		return "✅ " + strings.TrimSpace(c[6:]) + "\n", nil
	}
	if strings.HasPrefix(up, "ECHO ") {
		return "✅ " + strings.TrimSpace(c[5:]) + "\n", nil
	}
	return "❌ BASIC: unsupported command\n", nil
}
