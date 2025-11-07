package pilot

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// Executor handles comprehensive PILOT commands matching Rust reference.
// Supported:
//   - T:<text>  (type/display text)
//   - A:<var>   (accept input to variable)
//   - U:<var=expr> (use/assign variable)
//   - C:<condition> (compute/test condition)
//   - Y:<condition> (yes - execute if last condition true)
//   - N:<condition> (no - execute if last condition false)
//   - M:<pattern> (match pattern in last input)
//   - J:<label> (jump to label)
//   - L:<label> (define label)
//   - E: (end program)
//   - R:<text> (remark/comment)

type Executor struct {
	variables       map[string]string
	labels          map[string]int
	lastInput       string
	matchFlag       bool
	conditionResult bool
	currentLine     int
}

func New() *Executor {
	return &Executor{
		variables:       make(map[string]string),
		labels:          make(map[string]int),
		matchFlag:       false,
		conditionResult: false,
	}
}

func (e *Executor) Execute(command string) (string, error) {
	c := strings.TrimSpace(command)
	if len(c) < 2 || c[1] != ':' {
		return "", nil
	}

	cmdType := strings.ToUpper(string(c[0]))
	content := strings.TrimSpace(c[2:])

	switch cmdType {
	case "T":
		// Type/display text with variable interpolation
		text := e.interpolateVars(content)
		return fmt.Sprintf("ℹ️  %s\n", text), nil

	case "A":
		// Accept input to variable
		varName := content
		// In real implementation, would trigger input prompt
		e.variables[varName] = ""
		e.lastInput = ""
		return fmt.Sprintf("📝 INPUT to %s\n", varName), nil

	case "U":
		// Use/assign variable
		if strings.Contains(content, "=") {
			parts := strings.SplitN(content, "=", 2)
			varName := strings.TrimSpace(parts[0])
			value := strings.TrimSpace(parts[1])
			e.variables[varName] = e.evaluateExpression(value)
			return "", nil
		}
		// Just print variable value
		varName := content
		val := e.variables[varName]
		return fmt.Sprintf("ℹ️  %s = %s\n", varName, val), nil

	case "C":
		// Compute/test condition
		e.conditionResult = e.evaluateCondition(content)
		return "", nil

	case "Y":
		// Yes - execute if last condition/match true
		if e.conditionResult || e.matchFlag {
			return "✅ YES condition met\n", nil
		}
		return "", nil

	case "N":
		// No - execute if last condition/match false
		if !e.conditionResult && !e.matchFlag {
			return "✅ NO condition met\n", nil
		}
		return "", nil

	case "M":
		// Match pattern in last input
		pattern := content
		e.matchFlag = e.matchPattern(pattern, e.lastInput)
		return "", nil

	case "J":
		// Jump to label
		label := content
		if lineNum, ok := e.labels[label]; ok {
			e.currentLine = lineNum
			return fmt.Sprintf("🚀 JUMP to %s (line %d)\n", label, lineNum), nil
		}
		return fmt.Sprintf("❌ Label '%s' not found\n", label), nil

	case "L":
		// Define label
		label := content
		e.labels[label] = e.currentLine
		return "", nil

	case "E":
		// End program
		return "✅ Program ended\n", nil

	case "R":
		// Remark/comment
		return "", nil

	default:
		return fmt.Sprintf("❌ PILOT: unknown command '%s:'\n", cmdType), nil
	}
}

// RunProgram executes a multi-line PILOT program with label resolution and jumps
func (e *Executor) RunProgram(program string) string {
	// Reset state
	e.variables = make(map[string]string)
	e.labels = make(map[string]int)
	e.lastInput = ""
	e.matchFlag = false
	e.conditionResult = false

	lines := strings.Split(strings.ReplaceAll(program, "\r", ""), "\n")
	parsed := make([]string, 0, len(lines))

	// First pass: collect lines and labels
	for _, raw := range lines {
		t := strings.TrimSpace(raw)
		if t == "" {
			continue
		}
		parsed = append(parsed, t)

		// Check for label definition
		if len(t) >= 2 && t[1] == ':' && strings.ToUpper(string(t[0])) == "L" {
			label := strings.TrimSpace(t[2:])
			e.labels[label] = len(parsed) - 1
		}
	}

	// Second pass: execute
	out := &strings.Builder{}
	pc := 0
	steps := 0
	maxSteps := 10000

	for pc >= 0 && pc < len(parsed) && steps < maxSteps {
		steps++
		cmd := parsed[pc]

		if len(cmd) < 2 || cmd[1] != ':' {
			pc++
			continue
		}

		cmdType := strings.ToUpper(string(cmd[0]))
		content := strings.TrimSpace(cmd[2:])

		switch cmdType {
		case "T":
			text := e.interpolateVars(content)
			out.WriteString(text + "\n")
			pc++
		case "A":
			// Accept input (stub: set to empty)
			e.variables[content] = ""
			e.lastInput = ""
			pc++
		case "U":
			// Use/assign
			if strings.Contains(content, "=") {
				parts := strings.SplitN(content, "=", 2)
				varName := strings.TrimSpace(parts[0])
				value := strings.TrimSpace(parts[1])
				e.variables[varName] = e.evaluateExpression(value)
			}
			pc++
		case "C":
			e.conditionResult = e.evaluateCondition(content)
			pc++
		case "Y":
			// Execute next statement if condition/match true
			if e.conditionResult || e.matchFlag {
				pc++
			} else {
				pc += 2 // skip next
			}
		case "N":
			// Execute next statement if condition/match false
			if !e.conditionResult && !e.matchFlag {
				pc++
			} else {
				pc += 2 // skip next
			}
		case "M":
			e.matchFlag = e.matchPattern(content, e.lastInput)
			pc++
		case "J":
			// Jump to label
			label := content
			if lineNum, ok := e.labels[label]; ok {
				pc = lineNum
			} else {
				out.WriteString(fmt.Sprintf("❌ Label '%s' not found\n", label))
				pc++
			}
		case "L":
			// Label definition (no-op in execution)
			pc++
		case "E":
			// End program
			return out.String()
		case "R":
			// Comment
			pc++
		default:
			pc++
		}
	}

	if steps >= maxSteps {
		out.WriteString("❌ Stopped: too many steps\n")
	}
	return out.String()
}

// Interpolate variables in text (*VAR* syntax)
func (e *Executor) interpolateVars(text string) string {
	re := regexp.MustCompile(`\*(\w+)\*`)
	return re.ReplaceAllStringFunc(text, func(match string) string {
		varName := match[1 : len(match)-1]
		if val, ok := e.variables[varName]; ok {
			return val
		}
		return match
	})
}

// Evaluate simple expressions (numeric or string)
func (e *Executor) evaluateExpression(expr string) string {
	expr = strings.TrimSpace(expr)

	// Try numeric expression
	if val, err := strconv.ParseFloat(expr, 64); err == nil {
		return fmt.Sprintf("%g", val)
	}

	// Simple arithmetic
	if strings.Contains(expr, "+") {
		parts := strings.SplitN(expr, "+", 2)
		left, _ := strconv.ParseFloat(strings.TrimSpace(parts[0]), 64)
		right, _ := strconv.ParseFloat(strings.TrimSpace(parts[1]), 64)
		return fmt.Sprintf("%g", left+right)
	}
	if strings.Contains(expr, "-") {
		parts := strings.SplitN(expr, "-", 2)
		left, _ := strconv.ParseFloat(strings.TrimSpace(parts[0]), 64)
		right, _ := strconv.ParseFloat(strings.TrimSpace(parts[1]), 64)
		return fmt.Sprintf("%g", left-right)
	}
	if strings.Contains(expr, "*") {
		parts := strings.SplitN(expr, "*", 2)
		left, _ := strconv.ParseFloat(strings.TrimSpace(parts[0]), 64)
		right, _ := strconv.ParseFloat(strings.TrimSpace(parts[1]), 64)
		return fmt.Sprintf("%g", left*right)
	}
	if strings.Contains(expr, "/") {
		parts := strings.SplitN(expr, "/", 2)
		left, _ := strconv.ParseFloat(strings.TrimSpace(parts[0]), 64)
		right, _ := strconv.ParseFloat(strings.TrimSpace(parts[1]), 64)
		if right != 0 {
			return fmt.Sprintf("%g", left/right)
		}
	}

	// Check if it's a variable reference
	if val, ok := e.variables[expr]; ok {
		return val
	}

	// Return as-is (string literal)
	return strings.Trim(expr, "\"")
}

// Evaluate condition (supports =, >, <, >=, <=, <>)
func (e *Executor) evaluateCondition(condition string) bool {
	condition = strings.TrimSpace(condition)

	// Check for operators
	operators := []string{">=", "<=", "<>", "=", ">", "<"}
	for _, op := range operators {
		if strings.Contains(condition, op) {
			parts := strings.SplitN(condition, op, 2)
			left := e.evaluateExpression(strings.TrimSpace(parts[0]))
			right := e.evaluateExpression(strings.TrimSpace(parts[1]))

			// Try numeric comparison
			leftNum, leftErr := strconv.ParseFloat(left, 64)
			rightNum, rightErr := strconv.ParseFloat(right, 64)

			if leftErr == nil && rightErr == nil {
				switch op {
				case "=":
					return leftNum == rightNum
				case ">":
					return leftNum > rightNum
				case "<":
					return leftNum < rightNum
				case ">=":
					return leftNum >= rightNum
				case "<=":
					return leftNum <= rightNum
				case "<>":
					return leftNum != rightNum
				}
			} else {
				// String comparison
				switch op {
				case "=":
					return left == right
				case "<>":
					return left != right
				}
			}
		}
	}

	return false
}

// Match pattern against text (supports wildcards)
func (e *Executor) matchPattern(pattern, text string) bool {
	pattern = strings.ToLower(strings.TrimSpace(pattern))
	text = strings.ToLower(strings.TrimSpace(text))

	// Convert simple wildcard pattern to regex
	// * matches anything
	regexPattern := "^" + strings.ReplaceAll(regexp.QuoteMeta(pattern), "\\*", ".*") + "$"
	matched, _ := regexp.MatchString(regexPattern, text)
	return matched
}
