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
			return fmt.Sprintf("✅ YES condition met\n"), nil
		}
		return "", nil

	case "N":
		// No - execute if last condition/match false
		if !e.conditionResult && !e.matchFlag {
			return fmt.Sprintf("✅ NO condition met\n"), nil
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
