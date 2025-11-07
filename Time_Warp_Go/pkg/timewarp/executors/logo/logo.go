package logo

import (
	"fmt"
	"strconv"
	"strings"
)

// Executor handles a tiny subset of Logo-like commands.
// Supported:
//   - FORWARD/FD <n>
//   - RIGHT <deg>
//   - LEFT <deg>

type Executor struct{}

func New() *Executor { return &Executor{} }

func (e *Executor) Execute(command string) (string, error) {
	c := strings.TrimSpace(command)
	up := strings.ToUpper(c)

	valAfter := func(prefix string) string {
		return strings.TrimSpace(c[len(prefix):])
	}
	parseNum := func(s string) (float64, bool) {
		n, err := strconv.ParseFloat(strings.Fields(s)[0], 64)
		return n, err == nil
	}

	switch {
	case strings.HasPrefix(up, "FORWARD "):
		arg := valAfter("FORWARD ")
		if n, ok := parseNum(arg); ok {
			return fmt.Sprintf("🐢 FORWARD %.2f\n", n), nil
		}
		return "🐢 FORWARD (no distance)\n", nil

	case strings.HasPrefix(up, "FD "):
		arg := valAfter("FD ")
		if n, ok := parseNum(arg); ok {
			return fmt.Sprintf("🐢 FD %.2f\n", n), nil
		}
		return "🐢 FD (no distance)\n", nil

	case strings.HasPrefix(up, "RIGHT "):
		arg := valAfter("RIGHT ")
		if n, ok := parseNum(arg); ok {
			return fmt.Sprintf("🐢 RIGHT %.2f°\n", n), nil
		}
		return "🐢 RIGHT (no angle)\n", nil

	case strings.HasPrefix(up, "LEFT "):
		arg := valAfter("LEFT ")
		if n, ok := parseNum(arg); ok {
			return fmt.Sprintf("🐢 LEFT %.2f°\n", n), nil
		}
		return "🐢 LEFT (no angle)\n", nil
	}

	return "❌ LOGO: unsupported command\n", nil
}
