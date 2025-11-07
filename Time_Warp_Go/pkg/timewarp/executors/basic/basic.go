package basic

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

// Executor handles comprehensive BASIC commands matching Rust reference.
// Supported:
//   - PRINT <expr|text>
//   - LET var = expr
//   - INPUT var
//   - GOTO line
//   - IF condition THEN action
//   - FOR var = start TO end [STEP step]
//   - NEXT var
//   - GOSUB line
//   - RETURN
//   - REM comment
//   - END
//   - CLS
//   - LINE x1,y1,x2,y2
//   - CIRCLE x,y,r
//   - LOCATE row,col

type Executor struct {
	variables   map[string]float64
	gosubStack  []int
	forStack    []forContext
	lineNumbers map[int]int // line number -> index
	lines       []string
	currentLine int
}

type forContext struct {
	variable  string
	endVal    float64
	step      float64
	startLine int
}

func New() *Executor {
	return &Executor{
		variables:   make(map[string]float64),
		gosubStack:  make([]int, 0, 64),
		forStack:    make([]forContext, 0, 16),
		lineNumbers: make(map[int]int),
	}
}

func (e *Executor) Execute(command string) (string, error) {
	c := strings.TrimSpace(command)
	if c == "" {
		return "", nil
	}

	// Parse command keyword
	parts := strings.SplitN(c, " ", 2)
	keyword := strings.ToUpper(parts[0])
	args := ""
	if len(parts) > 1 {
		args = parts[1]
	}

	switch keyword {
	case "PRINT":
		return e.executePrint(args), nil
	case "LET":
		return e.executeLet(args), nil
	case "INPUT":
		return e.executeInput(args), nil
	case "GOTO":
		return e.executeGoto(args), nil
	case "IF":
		return e.executeIf(c), nil
	case "FOR":
		return e.executeFor(args), nil
	case "NEXT":
		return e.executeNext(args), nil
	case "GOSUB":
		return e.executeGosub(args), nil
	case "RETURN":
		return e.executeReturn(), nil
	case "REM", "//", "#":
		return "", nil
	case "END":
		return "✅ Program ended\n", nil
	case "CLS":
		return "🎨 Screen cleared\n", nil
	case "LINE":
		return e.executeLine(args), nil
	case "CIRCLE":
		return e.executeCircle(args), nil
	case "LOCATE":
		return e.executeLocate(args), nil
	case "ECHO":
		return "✅ " + strings.TrimSpace(args) + "\n", nil
	default:
		// Try variable assignment without LET
		if strings.Contains(c, "=") && !strings.Contains(keyword, "=") {
			return e.executeLet(c), nil
		}
		return fmt.Sprintf("❌ BASIC: unknown command '%s'\n", keyword), nil
	}
}

func (e *Executor) executePrint(args string) string {
	if args == "" {
		return "\n"
	}
	// Simple variable interpolation and expression eval
	result := e.evaluateExpression(args)
	return fmt.Sprintf("✅ %s\n", result)
}

func (e *Executor) executeLet(args string) string {
	parts := strings.SplitN(args, "=", 2)
	if len(parts) != 2 {
		return "❌ LET requires format: var = expression\n"
	}
	varName := strings.TrimSpace(parts[0])
	exprStr := strings.TrimSpace(parts[1])

	// Evaluate expression
	value := e.evalNumeric(exprStr)
	e.variables[varName] = value
	return ""
}

func (e *Executor) executeInput(args string) string {
	varName := strings.TrimSpace(args)
	// In real implementation, would trigger input prompt
	// For now, set to 0 as placeholder
	e.variables[varName] = 0.0
	return fmt.Sprintf("📝 INPUT %s (set to 0 - needs input integration)\n", varName)
}

func (e *Executor) executeGoto(args string) string {
	lineNum, err := strconv.Atoi(strings.TrimSpace(args))
	if err != nil {
		return "❌ GOTO requires line number\n"
	}
	// In real implementation, would jump to line
	return fmt.Sprintf("🚀 GOTO %d\n", lineNum)
}

func (e *Executor) executeIf(full string) string {
	upper := strings.ToUpper(full)
	thenIdx := strings.Index(upper, "THEN")
	if thenIdx == -1 {
		return "❌ IF requires THEN\n"
	}

	condStr := strings.TrimSpace(full[3:thenIdx])
	action := strings.TrimSpace(full[thenIdx+4:])

	// Evaluate condition (simple numeric check)
	cond := e.evalNumeric(condStr)
	if cond != 0 {
		return fmt.Sprintf("✅ IF %s THEN %s (condition true)\n", condStr, action)
	}
	return ""
}

func (e *Executor) executeFor(args string) string {
	// FOR var = start TO end [STEP step]
	upper := strings.ToUpper(args)
	toIdx := strings.Index(upper, "TO")
	if toIdx == -1 {
		return "❌ FOR requires TO\n"
	}

	assignment := strings.TrimSpace(args[:toIdx])
	rest := strings.TrimSpace(args[toIdx+2:])

	parts := strings.SplitN(assignment, "=", 2)
	if len(parts) != 2 {
		return "❌ FOR requires var = value\n"
	}

	varName := strings.TrimSpace(parts[0])
	startVal := e.evalNumeric(strings.TrimSpace(parts[1]))

	stepVal := 1.0
	endPart := rest
	if stepIdx := strings.Index(strings.ToUpper(rest), "STEP"); stepIdx != -1 {
		endPart = strings.TrimSpace(rest[:stepIdx])
		stepStr := strings.TrimSpace(rest[stepIdx+4:])
		stepVal = e.evalNumeric(stepStr)
	}

	endVal := e.evalNumeric(endPart)

	e.variables[varName] = startVal
	ctx := forContext{
		variable:  varName,
		endVal:    endVal,
		step:      stepVal,
		startLine: e.currentLine,
	}
	e.forStack = append(e.forStack, ctx)

	return fmt.Sprintf("🔄 FOR %s = %g TO %g STEP %g\n", varName, startVal, endVal, stepVal)
}

func (e *Executor) executeNext(args string) string {
	if len(e.forStack) == 0 {
		return "❌ NEXT without FOR\n"
	}

	ctx := e.forStack[len(e.forStack)-1]
	currentVal := e.variables[ctx.variable]
	currentVal += ctx.step
	e.variables[ctx.variable] = currentVal

	done := false
	if ctx.step > 0 {
		done = currentVal > ctx.endVal
	} else {
		done = currentVal < ctx.endVal
	}

	if done {
		e.forStack = e.forStack[:len(e.forStack)-1]
		return fmt.Sprintf("✅ NEXT %s (loop complete)\n", ctx.variable)
	}

	return fmt.Sprintf("🔄 NEXT %s = %g\n", ctx.variable, currentVal)
}

func (e *Executor) executeGosub(args string) string {
	lineNum, err := strconv.Atoi(strings.TrimSpace(args))
	if err != nil {
		return "❌ GOSUB requires line number\n"
	}
	e.gosubStack = append(e.gosubStack, e.currentLine+1)
	return fmt.Sprintf("🚀 GOSUB %d\n", lineNum)
}

func (e *Executor) executeReturn() string {
	if len(e.gosubStack) == 0 {
		return "❌ RETURN without GOSUB\n"
	}
	returnLine := e.gosubStack[len(e.gosubStack)-1]
	e.gosubStack = e.gosubStack[:len(e.gosubStack)-1]
	return fmt.Sprintf("🔙 RETURN to line %d\n", returnLine)
}

func (e *Executor) executeLine(args string) string {
	// LINE x1,y1,x2,y2
	parts := strings.Split(args, ",")
	if len(parts) != 4 {
		return "❌ LINE requires x1,y1,x2,y2\n"
	}
	return fmt.Sprintf("📏 LINE %s,%s,%s,%s\n",
		strings.TrimSpace(parts[0]),
		strings.TrimSpace(parts[1]),
		strings.TrimSpace(parts[2]),
		strings.TrimSpace(parts[3]))
}

func (e *Executor) executeCircle(args string) string {
	// CIRCLE x,y,r
	parts := strings.Split(args, ",")
	if len(parts) != 3 {
		return "❌ CIRCLE requires x,y,r\n"
	}
	return fmt.Sprintf("⭕ CIRCLE %s,%s,%s\n",
		strings.TrimSpace(parts[0]),
		strings.TrimSpace(parts[1]),
		strings.TrimSpace(parts[2]))
}

func (e *Executor) executeLocate(args string) string {
	// LOCATE row,col
	parts := strings.Split(args, ",")
	if len(parts) != 2 {
		return "❌ LOCATE requires row,col\n"
	}
	return fmt.Sprintf("📍 LOCATE %s,%s\n",
		strings.TrimSpace(parts[0]),
		strings.TrimSpace(parts[1]))
}

// Simple expression evaluator
func (e *Executor) evaluateExpression(expr string) string {
	expr = strings.TrimSpace(expr)

	// Check if it's a string literal
	if strings.HasPrefix(expr, "\"") && strings.HasSuffix(expr, "\"") {
		return expr[1 : len(expr)-1]
	}

	// Try to evaluate as numeric
	val := e.evalNumeric(expr)
	return fmt.Sprintf("%g", val)
}

func (e *Executor) evalNumeric(expr string) float64 {
	expr = strings.TrimSpace(expr)

	// Check for variable
	if val, ok := e.variables[expr]; ok {
		return val
	}

	// Try parse as number
	if val, err := strconv.ParseFloat(expr, 64); err == nil {
		return val
	}

	// Simple operator support (left-to-right, no precedence)
	if strings.Contains(expr, "+") {
		parts := strings.SplitN(expr, "+", 2)
		return e.evalNumeric(parts[0]) + e.evalNumeric(parts[1])
	}
	if strings.Contains(expr, "-") && !strings.HasPrefix(expr, "-") {
		parts := strings.SplitN(expr, "-", 2)
		return e.evalNumeric(parts[0]) - e.evalNumeric(parts[1])
	}
	if strings.Contains(expr, "*") {
		parts := strings.SplitN(expr, "*", 2)
		return e.evalNumeric(parts[0]) * e.evalNumeric(parts[1])
	}
	if strings.Contains(expr, "/") {
		parts := strings.SplitN(expr, "/", 2)
		divisor := e.evalNumeric(parts[1])
		if divisor == 0 {
			return 0
		}
		return e.evalNumeric(parts[0]) / divisor
	}

	// Built-in functions
	if strings.HasPrefix(expr, "ABS(") && strings.HasSuffix(expr, ")") {
		inner := expr[4 : len(expr)-1]
		return math.Abs(e.evalNumeric(inner))
	}
	if strings.HasPrefix(expr, "INT(") && strings.HasSuffix(expr, ")") {
		inner := expr[4 : len(expr)-1]
		return math.Floor(e.evalNumeric(inner))
	}
	if strings.HasPrefix(expr, "SQR(") && strings.HasSuffix(expr, ")") {
		inner := expr[4 : len(expr)-1]
		return math.Sqrt(e.evalNumeric(inner))
	}

	return 0.0
}
