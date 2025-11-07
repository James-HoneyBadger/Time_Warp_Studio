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

// RunProgram executes a multi-line BASIC program with optional line numbers
// and control flow (GOTO, IF ... THEN <line>, FOR/NEXT, GOSUB/RETURN).
// It returns the concatenated output with emoji prefixes per project convention.
func (e *Executor) RunProgram(program string) string {
	// reset state
	e.variables = make(map[string]float64)
	e.gosubStack = e.gosubStack[:0]
	e.forStack = e.forStack[:0]
	e.lineNumbers = make(map[int]int)
	e.lines = make([]string, 0, 256)
	e.currentLine = 0

	type line struct {
		num  int
		hasN bool
		text string
	}
	parsed := make([]line, 0, 256)
	// parse lines and map numbers
	for _, raw := range strings.Split(strings.ReplaceAll(program, "\r", ""), "\n") {
		t := strings.TrimSpace(raw)
		if t == "" {
			continue
		}
		// check for leading number
		n := -1
		has := false
		sp := strings.SplitN(t, " ", 2)
		if len(sp) > 0 {
			if v, err := strconv.Atoi(sp[0]); err == nil {
				n = v
				has = true
				if len(sp) > 1 {
					t = strings.TrimSpace(sp[1])
				} else {
					t = ""
				}
			}
		}
		idx := len(parsed)
		if has {
			e.lineNumbers[n] = idx
		}
		parsed = append(parsed, line{num: n, hasN: has, text: t})
	}

	// program counter loop
	out := &strings.Builder{}
	pc := 0
	steps := 0
	maxSteps := 20000

	getIndex := func(target int) (int, bool) {
		idx, ok := e.lineNumbers[target]
		return idx, ok
	}

	for pc >= 0 && pc < len(parsed) && steps < maxSteps {
		steps++
		cmd := strings.TrimSpace(parsed[pc].text)
		up := strings.ToUpper(cmd)
		if cmd == "" || strings.HasPrefix(up, "REM") {
			pc++
			continue
		}
		switch {
		case up == "END":
			// terminate
			return out.String()
		case up == "CLS":
			// clear screen: no textual output in batch mode
			pc++
		case strings.HasPrefix(up, "PRINT"):
			o := e.executePrint(strings.TrimSpace(cmd[5:]))
			out.WriteString(o)
			pc++
		case strings.HasPrefix(up, "LET ") || strings.Contains(cmd, "="):
			// assignment with/without LET
			assign := cmd
			if strings.HasPrefix(up, "LET ") {
				assign = strings.TrimSpace(cmd[4:])
			}
			out.WriteString(e.executeLet(assign))
			pc++
		case strings.HasPrefix(up, "INPUT "):
			out.WriteString(e.executeInput(strings.TrimSpace(cmd[6:])))
			pc++
		case strings.HasPrefix(up, "GOTO "):
			arg := strings.TrimSpace(cmd[4:])
			ln, err := strconv.Atoi(arg)
			if err != nil {
				out.WriteString("❌ GOTO requires line number\n")
				pc++
				break
			}
			if idx, ok := getIndex(ln); ok {
				pc = idx
			} else {
				out.WriteString("❌ Unknown line\n")
				pc++
			}
		case strings.HasPrefix(up, "IF "):
			// IF <cond> THEN <line>
			thenIdx := strings.Index(strings.ToUpper(cmd), "THEN")
			if thenIdx < 0 {
				out.WriteString("❌ IF requires THEN\n")
				pc++
				break
			}
			condStr := strings.TrimSpace(cmd[3:thenIdx])
			tgt := strings.TrimSpace(cmd[thenIdx+4:])
			cond := e.evalNumeric(condStr) != 0
			if cond {
				if ln, err := strconv.Atoi(tgt); err == nil {
					if idx, ok := getIndex(ln); ok {
						pc = idx
						break
					}
				}
			}
			pc++
		case strings.HasPrefix(up, "FOR "):
			rest := strings.TrimSpace(cmd[4:])
			// var = start TO end [STEP step]
			eqIdx := strings.Index(rest, "=")
			if eqIdx < 0 {
				out.WriteString("❌ FOR requires var = start TO end\n")
				pc++
				break
			}
			vname := strings.TrimSpace(rest[:eqIdx])
			afterEq := strings.TrimSpace(rest[eqIdx+1:])
			toIdx := strings.Index(strings.ToUpper(afterEq), "TO")
			if toIdx < 0 {
				out.WriteString("❌ FOR requires TO\n")
				pc++
				break
			}
			startStr := strings.TrimSpace(afterEq[:toIdx])
			afterTo := strings.TrimSpace(afterEq[toIdx+2:])
			stepVal := 1.0
			endStr := afterTo
			if stIdx := strings.Index(strings.ToUpper(afterTo), "STEP"); stIdx >= 0 {
				endStr = strings.TrimSpace(afterTo[:stIdx])
				stepStr := strings.TrimSpace(afterTo[stIdx+4:])
				stepVal = e.evalNumeric(stepStr)
			}
			startVal := e.evalNumeric(startStr)
			endVal := e.evalNumeric(endStr)
			e.variables[vname] = startVal
			// remember next line as loop start
			e.forStack = append(e.forStack, forContext{
				variable:  vname,
				endVal:    endVal,
				step:      stepVal,
				startLine: pc + 1,
			})
			// no output for FOR control line
			pc++
		case strings.HasPrefix(up, "NEXT"):
			if len(e.forStack) == 0 {
				out.WriteString("❌ NEXT without FOR\n")
				pc++
				break
			}
			ctx := e.forStack[len(e.forStack)-1]
			// optional variable name check
			parts := strings.Fields(cmd)
			if len(parts) > 1 && !strings.EqualFold(parts[1], ctx.variable) {
				out.WriteString("❌ NEXT wrong variable\n")
				pc++
				break
			}
			cur := e.variables[ctx.variable] + ctx.step
			e.variables[ctx.variable] = cur
			cont := (ctx.step >= 0 && cur <= ctx.endVal) || (ctx.step < 0 && cur >= ctx.endVal)
			if cont {
				pc = ctx.startLine
			} else {
				e.forStack = e.forStack[:len(e.forStack)-1]
				// loop complete; no output
				pc++
			}
		case strings.HasPrefix(up, "GOSUB "):
			arg := strings.TrimSpace(cmd[6:])
			ln, err := strconv.Atoi(arg)
			if err != nil {
				out.WriteString("❌ GOSUB requires line number\n")
				pc++
				break
			}
			e.gosubStack = append(e.gosubStack, pc+1)
			if idx, ok := getIndex(ln); ok {
				pc = idx
			} else {
				out.WriteString("❌ Unknown line\n")
				pc++
			}
		case up == "RETURN":
			if len(e.gosubStack) == 0 {
				out.WriteString("❌ RETURN without GOSUB\n")
				pc++
				break
			}
			ret := e.gosubStack[len(e.gosubStack)-1]
			e.gosubStack = e.gosubStack[:len(e.gosubStack)-1]
			pc = ret
		case strings.HasPrefix(up, "LINE "):
			out.WriteString("📏 LINE " + strings.TrimSpace(cmd[4:]) + "\n")
			pc++
		case strings.HasPrefix(up, "CIRCLE "):
			out.WriteString("⭕ CIRCLE " + strings.TrimSpace(cmd[6:]) + "\n")
			pc++
		case strings.HasPrefix(up, "LOCATE "):
			out.WriteString("📍 LOCATE " + strings.TrimSpace(cmd[6:]) + "\n")
			pc++
		default:
			// unknown falls back to expression or ignore
			out.WriteString(fmt.Sprintf("❌ BASIC: unknown command '%s'\n", up))
			pc++
		}
	}
	if steps >= maxSteps {
		out.WriteString("❌ Stopped: too many steps\n")
	}
	return out.String()
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
