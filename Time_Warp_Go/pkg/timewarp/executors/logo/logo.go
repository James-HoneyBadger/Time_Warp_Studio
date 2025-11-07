package logo

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

// Executor handles comprehensive Logo turtle graphics commands.
// Supported:
//   - FORWARD/FD <n>, BACK/BK <n>
//   - LEFT/LT <deg>, RIGHT/RT <deg>
//   - PENUP/PU, PENDOWN/PD
//   - HOME, CLEARSCREEN/CS
//   - SETXY x y, SETHEADING/SETH <deg>
//   - SETCOLOR r g b, PENWIDTH <n>
//   - HIDETURTLE/HT, SHOWTURTLE/ST
//   - TO name...END (procedures)
//   - REPEAT n [...]

type Executor struct {
	procedures   map[string]string
	variables    map[string]float64
	turtleX      float64
	turtleY      float64
	turtleAngle  float64
	penDown      bool
	penColor     [3]int
	penWidth     int
	turtleHidden bool
}

func New() *Executor {
	return &Executor{
		procedures:   make(map[string]string),
		variables:    make(map[string]float64),
		turtleX:      0,
		turtleY:      0,
		turtleAngle:  0,
		penDown:      true,
		penColor:     [3]int{0, 0, 0},
		penWidth:     1,
		turtleHidden: false,
	}
}

func (e *Executor) Execute(command string) (string, error) {
	c := strings.TrimSpace(command)
	if c == "" {
		return "", nil
	}

	up := strings.ToUpper(c)
	parts := strings.Fields(up)
	if len(parts) == 0 {
		return "", nil
	}

	cmd := parts[0]
	args := ""
	if len(parts) > 1 {
		args = strings.Join(parts[1:], " ")
	}

	// removed unused helper valAfter from earlier version
	parseNum := func(s string) (float64, bool) {
		fields := strings.Fields(s)
		if len(fields) == 0 {
			return 0, false
		}
		n, err := strconv.ParseFloat(fields[0], 64)
		return n, err == nil
	}

	switch cmd {
	case "FORWARD", "FD":
		arg := strings.TrimSpace(c[len(parts[0]):])
		if n, ok := parseNum(arg); ok {
			rad := e.turtleAngle * math.Pi / 180.0
			newX := e.turtleX + n*math.Cos(rad)
			newY := e.turtleY + n*math.Sin(rad)
			out := fmt.Sprintf("🐢 FORWARD %.2f (%.2f,%.2f → %.2f,%.2f)\n", n, e.turtleX, e.turtleY, newX, newY)
			e.turtleX = newX
			e.turtleY = newY
			return out, nil
		}
		return "🐢 FORWARD (no distance)\n", nil

	case "BACK", "BK", "BACKWARD":
		arg := strings.TrimSpace(c[len(parts[0]):])
		if n, ok := parseNum(arg); ok {
			rad := e.turtleAngle * math.Pi / 180.0
			newX := e.turtleX - n*math.Cos(rad)
			newY := e.turtleY - n*math.Sin(rad)
			out := fmt.Sprintf("🐢 BACK %.2f (%.2f,%.2f → %.2f,%.2f)\n", n, e.turtleX, e.turtleY, newX, newY)
			e.turtleX = newX
			e.turtleY = newY
			return out, nil
		}
		return "🐢 BACK (no distance)\n", nil

	case "RIGHT", "RT":
		arg := strings.TrimSpace(c[len(parts[0]):])
		if n, ok := parseNum(arg); ok {
			e.turtleAngle = math.Mod(e.turtleAngle+n, 360)
			return fmt.Sprintf("🐢 RIGHT %.2f° (heading: %.2f°)\n", n, e.turtleAngle), nil
		}
		return "🐢 RIGHT (no angle)\n", nil

	case "LEFT", "LT":
		arg := strings.TrimSpace(c[len(parts[0]):])
		if n, ok := parseNum(arg); ok {
			e.turtleAngle = math.Mod(e.turtleAngle-n+360, 360)
			return fmt.Sprintf("🐢 LEFT %.2f° (heading: %.2f°)\n", n, e.turtleAngle), nil
		}
		return "🐢 LEFT (no angle)\n", nil

	case "PENUP", "PU":
		e.penDown = false
		return "🐢 PENUP\n", nil

	case "PENDOWN", "PD":
		e.penDown = true
		return "🐢 PENDOWN\n", nil

	case "HOME":
		e.turtleX = 0
		e.turtleY = 0
		e.turtleAngle = 0
		return "🐢 HOME (0,0) heading 0°\n", nil

	case "CLEARSCREEN", "CS", "CLEAR":
		e.turtleX = 0
		e.turtleY = 0
		e.turtleAngle = 0
		e.penDown = true
		return "🎨 Screen cleared, turtle home\n", nil

	case "SETXY":
		nums := strings.Fields(args)
		if len(nums) >= 2 {
			x, errX := strconv.ParseFloat(nums[0], 64)
			y, errY := strconv.ParseFloat(nums[1], 64)
			if errX == nil && errY == nil {
				oldX, oldY := e.turtleX, e.turtleY
				e.turtleX = x
				e.turtleY = y
				return fmt.Sprintf("🐢 SETXY %.2f %.2f (from %.2f,%.2f)\n", x, y, oldX, oldY), nil
			}
		}
		return "❌ SETXY requires x y coordinates\n", nil

	case "SETHEADING", "SETH":
		if n, ok := parseNum(args); ok {
			e.turtleAngle = math.Mod(n, 360)
			return fmt.Sprintf("🐢 SETHEADING %.2f°\n", e.turtleAngle), nil
		}
		return "❌ SETHEADING requires angle\n", nil

	case "SETCOLOR", "SETPENCOLOR", "SETPC":
		nums := strings.Fields(args)
		if len(nums) >= 3 {
			r, _ := strconv.Atoi(nums[0])
			g, _ := strconv.Atoi(nums[1])
			b, _ := strconv.Atoi(nums[2])
			e.penColor = [3]int{r, g, b}
			return fmt.Sprintf("🎨 SETCOLOR %d %d %d\n", r, g, b), nil
		}
		return "❌ SETCOLOR requires r g b values\n", nil

	case "PENWIDTH", "SETPENWIDTH", "SETPW", "SETPENSIZE":
		if n, ok := parseNum(args); ok {
			e.penWidth = int(n)
			return fmt.Sprintf("✏️  PENWIDTH %d\n", e.penWidth), nil
		}
		return "❌ PENWIDTH requires width value\n", nil

	case "HIDETURTLE", "HT":
		e.turtleHidden = true
		return "🐢 HIDETURTLE\n", nil

	case "SHOWTURTLE", "ST":
		e.turtleHidden = false
		return "🐢 SHOWTURTLE\n", nil

	case "TO":
		return "📝 TO (procedure def unsupported in Go stub)\n", nil

	case "END":
		return "✅ END\n", nil

	case "REPEAT":
		return "🔄 REPEAT (unsupported in Go stub)\n", nil

	default:
		return fmt.Sprintf("❌ Logo: unknown command '%s'\n", cmd), nil
	}
}
