// SPDX-License-Identifier: MIT
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

	events []TurtleEvent
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
		events:       nil,
	}
}

// TurtleEventType enumerates turtle event kinds for structured rendering.
type TurtleEventType string

const (
	EventMove       TurtleEventType = "move"        // linear move; may draw
	EventPen                        = "pen"         // pen up/down
	EventHome                       = "home"        // go to origin, reset heading
	EventClear                      = "clear"       // clear screen and home
	EventSetHeading                 = "set_heading" // set absolute heading
	EventSetXY                      = "set_xy"      // teleport without drawing
	EventSetColor                   = "set_color"   // set pen color
	EventSetWidth                   = "set_width"   // set pen width
	EventShowHide                   = "show_hide"   // toggle visibility
)

// TurtleEvent is emitted for each Logo action to aid GUI rendering.
type TurtleEvent struct {
	Type   TurtleEventType
	FromX  float64
	FromY  float64
	ToX    float64
	ToY    float64
	Draw   bool
	Color  [3]int
	Width  int
	Down   bool // for EventPen
	Angle  float64
	Hidden bool
}

// ClearEvents drops any pending events (call before a new run if desired).
func (e *Executor) ClearEvents() { e.events = nil }

// PopEvents returns pending events and clears the buffer.
func (e *Executor) PopEvents() []TurtleEvent {
	ev := e.events
	e.events = nil
	return ev
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
			// event
			e.events = append(e.events, TurtleEvent{
				Type:  EventMove,
				FromX: e.turtleX, FromY: e.turtleY, ToX: newX, ToY: newY,
				Draw: e.penDown, Color: e.penColor, Width: e.penWidth,
				Angle: e.turtleAngle,
			})
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
			e.events = append(e.events, TurtleEvent{
				Type:  EventMove,
				FromX: e.turtleX, FromY: e.turtleY, ToX: newX, ToY: newY,
				Draw: e.penDown, Color: e.penColor, Width: e.penWidth,
				Angle: e.turtleAngle,
			})
			e.turtleX = newX
			e.turtleY = newY
			return out, nil
		}
		return "🐢 BACK (no distance)\n", nil

	case "RIGHT", "RT":
		arg := strings.TrimSpace(c[len(parts[0]):])
		if n, ok := parseNum(arg); ok {
			e.turtleAngle = math.Mod(e.turtleAngle+n, 360)
			out := fmt.Sprintf("🐢 RIGHT %.2f° (heading: %.2f°)\n", n, e.turtleAngle)
			e.events = append(e.events, TurtleEvent{Type: EventSetHeading, Angle: e.turtleAngle})
			return out, nil
		}
		return "🐢 RIGHT (no angle)\n", nil

	case "LEFT", "LT":
		arg := strings.TrimSpace(c[len(parts[0]):])
		if n, ok := parseNum(arg); ok {
			e.turtleAngle = math.Mod(e.turtleAngle-n+360, 360)
			out := fmt.Sprintf("🐢 LEFT %.2f° (heading: %.2f°)\n", n, e.turtleAngle)
			e.events = append(e.events, TurtleEvent{Type: EventSetHeading, Angle: e.turtleAngle})
			return out, nil
		}
		return "🐢 LEFT (no angle)\n", nil

	case "PENUP", "PU":
		e.penDown = false
		e.events = append(e.events, TurtleEvent{Type: EventPen, Down: false})
		return "🐢 PENUP\n", nil

	case "PENDOWN", "PD":
		e.penDown = true
		e.events = append(e.events, TurtleEvent{Type: EventPen, Down: true})
		return "🐢 PENDOWN\n", nil

	case "HOME":
		e.turtleX = 0
		e.turtleY = 0
		e.turtleAngle = 0
		e.events = append(e.events, TurtleEvent{Type: EventHome})
		return "🐢 HOME (0,0) heading 0°\n", nil

	case "CLEARSCREEN", "CS", "CLEAR":
		e.turtleX = 0
		e.turtleY = 0
		e.turtleAngle = 0
		e.penDown = true
		e.events = append(e.events, TurtleEvent{Type: EventClear})
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
				e.events = append(e.events, TurtleEvent{Type: EventSetXY, FromX: oldX, FromY: oldY, ToX: x, ToY: y})
				return fmt.Sprintf("🐢 SETXY %.2f %.2f (from %.2f,%.2f)\n", x, y, oldX, oldY), nil
			}
		}
		return "❌ SETXY requires x y coordinates\n", nil

	case "SETHEADING", "SETH":
		if n, ok := parseNum(args); ok {
			e.turtleAngle = math.Mod(n, 360)
			e.events = append(e.events, TurtleEvent{Type: EventSetHeading, Angle: e.turtleAngle})
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
			e.events = append(e.events, TurtleEvent{Type: EventSetColor, Color: e.penColor})
			return fmt.Sprintf("🎨 SETCOLOR %d %d %d\n", r, g, b), nil
		}
		return "❌ SETCOLOR requires r g b values\n", nil

	case "PENWIDTH", "SETPENWIDTH", "SETPW", "SETPENSIZE":
		if n, ok := parseNum(args); ok {
			e.penWidth = int(n)
			e.events = append(e.events, TurtleEvent{Type: EventSetWidth, Width: e.penWidth})
			return fmt.Sprintf("✏️  PENWIDTH %d\n", e.penWidth), nil
		}
		return "❌ PENWIDTH requires width value\n", nil

	case "HIDETURTLE", "HT":
		e.turtleHidden = true
		e.events = append(e.events, TurtleEvent{Type: EventShowHide, Hidden: true})
		return "🐢 HIDETURTLE\n", nil

	case "SHOWTURTLE", "ST":
		e.turtleHidden = false
		e.events = append(e.events, TurtleEvent{Type: EventShowHide, Hidden: false})
		return "🐢 SHOWTURTLE\n", nil

	case "TO":
		return "📝 TO (procedure def unsupported in Go stub)\n", nil

	case "END":
		return "✅ END\n", nil

	case "REPEAT":
		// Syntax: REPEAT n [ commands ]
		left := strings.Index(up, "[")
		right := strings.LastIndex(up, "]")
		if left == -1 || right == -1 || right <= left {
			return "❌ REPEAT syntax: REPEAT n [ commands ]\n", nil
		}
		countStr := strings.TrimSpace(up[len("REPEAT"):left])
		iters, err := strconv.Atoi(strings.Fields(countStr)[0])
		if err != nil || iters < 0 {
			return "❌ REPEAT requires a non-negative count\n", nil
		}
		inner := strings.TrimSpace(command[left+1 : right])
		if inner == "" {
			return "❌ REPEAT missing command block\n", nil
		}
		// naive tokenizer; nested REPEAT inside block is not supported in this pass
		toks := strings.Fields(inner)
		expectedArgs := func(name string) int {
			switch name {
			case "FORWARD", "FD", "BACK", "BK", "BACKWARD":
				return 1
			case "RIGHT", "RT", "LEFT", "LT":
				return 1
			case "SETXY":
				return 2
			case "SETHEADING", "SETH":
				return 1
			case "SETCOLOR", "SETPENCOLOR", "SETPC":
				return 3
			case "PENWIDTH", "SETPENWIDTH", "SETPW", "SETPENSIZE":
				return 1
			case "PENUP", "PU", "PENDOWN", "PD", "HOME", "CLEARSCREEN", "CS", "CLEAR", "HIDETURTLE", "HT", "SHOWTURTLE", "ST":
				return 0
			default:
				return 0
			}
		}
		runOnce := func() string {
			var b strings.Builder
			for i := 0; i < len(toks); {
				name := strings.ToUpper(toks[i])
				if name == "REPEAT" {
					b.WriteString("❌ Nested REPEAT not supported\n")
					i++
					continue
				}
				i++
				argc := expectedArgs(name)
				args := []string{}
				for j := 0; j < argc && i < len(toks); j++ {
					args = append(args, toks[i])
					i++
				}
				line := strings.TrimSpace(strings.Join(append([]string{name}, args...), " "))
				if line == "" {
					continue
				}
				out, _ := e.Execute(line)
				b.WriteString(out)
			}
			return b.String()
		}
		var out strings.Builder
		out.WriteString(fmt.Sprintf("🔄 REPEAT %d [ %s ]\n", iters, inner))
		for i := 0; i < iters; i++ {
			out.WriteString(runOnce())
		}
		return out.String(), nil

	default:
		return fmt.Sprintf("❌ Logo: unknown command '%s'\n", cmd), nil
	}
}

// GetTurtleState returns current turtle position and state for rendering
func (e *Executor) GetTurtleState() (x, y, angle float64, penDown bool, color [3]int, width int, hidden bool) {
	return e.turtleX, e.turtleY, e.turtleAngle, e.penDown, e.penColor, e.penWidth, e.turtleHidden
}

// GetTurtleLines returns lines drawn (for canvas rendering)
type TurtleLine struct {
	X1, Y1, X2, Y2 float64
	Color          [3]int
	Width          int
}

// Note: To implement full turtle graphics visualization, we'd need to track
// all drawing operations. For now, this provides state access.
