package pilot

import (
	"strings"
)

// Executor handles a tiny subset of PILOT-like commands.
// Supported:
//   - T:<text>  (teacher/message)
//   - A:<text>  (ask/prompt)

type Executor struct{}

func New() *Executor { return &Executor{} }

func (e *Executor) Execute(command string) (string, error) {
	c := strings.TrimSpace(command)
	up := strings.ToUpper(c)
	if strings.HasPrefix(up, "T:") {
		return "ℹ️  " + strings.TrimSpace(c[2:]) + "\n", nil
	}
	if strings.HasPrefix(up, "A:") {
		return "📝 " + strings.TrimSpace(c[2:]) + "\n", nil
	}
	return "❌ PILOT: unsupported command\n", nil
}
