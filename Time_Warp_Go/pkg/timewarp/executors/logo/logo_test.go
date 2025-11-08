package logo

import (
	"strings"
	"testing"
)

// TestExecutor_Forward tests FORWARD/FD command
func TestExecutor_Forward(t *testing.T) {
	tests := []struct {
		name    string
		command string
	}{
		{"forward full", "FORWARD 100"},
		{"forward short", "FD 50"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := New()
			initialX := e.turtleX
			initialY := e.turtleY
			result, err := e.Execute(tt.command)
			if err != nil {
				t.Fatalf("Execute() error = %v", err)
			}
			if !strings.Contains(result, "🐢") {
				t.Errorf("FORWARD should contain turtle emoji: %q", result)
			}
			// Turtle should have moved (angle 0 = right, so X or Y changes)
			if e.turtleX == initialX && e.turtleY == initialY {
				t.Errorf("FORWARD did not move turtle: still at (%v,%v)", initialX, initialY)
			}
		})
	}
}

// TestExecutor_Back tests BACK/BK command
func TestExecutor_Back(t *testing.T) {
	e := New()
	initialX := e.turtleX
	initialY := e.turtleY
	result, err := e.Execute("BACK 50")
	if err != nil {
		t.Fatalf("Execute(BACK) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("BACK should contain turtle emoji: %q", result)
	}
	// Turtle should have moved backward
	if e.turtleX == initialX && e.turtleY == initialY {
		t.Errorf("BACK did not move turtle: still at (%v,%v)", initialX, initialY)
	}
}

// TestExecutor_LeftRight tests LEFT/LT and RIGHT/RT commands
func TestExecutor_LeftRight(t *testing.T) {
	e := New()
	initialAngle := e.turtleAngle

	// Turn left 90 degrees
	result, err := e.Execute("LEFT 90")
	if err != nil {
		t.Fatalf("Execute(LEFT) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("LEFT should contain turtle emoji: %q", result)
	}
	if e.turtleAngle == initialAngle {
		t.Errorf("LEFT did not change angle: still %v", initialAngle)
	}

	// Turn right 45 degrees
	currentAngle := e.turtleAngle
	result, err = e.Execute("RT 45")
	if err != nil {
		t.Fatalf("Execute(RT) error = %v", err)
	}
	if !strings.Contains(result, "RIGHT") {
		t.Errorf("RIGHT output missing: %q", result)
	}
	if e.turtleAngle == currentAngle {
		t.Errorf("RIGHT did not change angle: still %v", currentAngle)
	}
}

// TestExecutor_PenUpDown tests PENUP/PU and PENDOWN/PD commands
func TestExecutor_PenUpDown(t *testing.T) {
	e := New()

	// Initially pen is down
	if !e.penDown {
		t.Error("Pen should initially be down")
	}

	// Pen up
	result, err := e.Execute("PENUP")
	if err != nil {
		t.Fatalf("Execute(PENUP) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("PENUP should contain turtle emoji: %q", result)
	}
	if e.penDown {
		t.Error("PENUP should set penDown to false")
	}

	// Pen down
	result, err = e.Execute("PD")
	if err != nil {
		t.Fatalf("Execute(PD) error = %v", err)
	}
	if !strings.Contains(result, "PENDOWN") {
		t.Errorf("PENDOWN output missing: %q", result)
	}
	if !e.penDown {
		t.Error("PENDOWN should set penDown to true")
	}
}

// TestExecutor_Home tests HOME command
func TestExecutor_Home(t *testing.T) {
	e := New()

	// Move turtle away from origin
	e.turtleX = 100
	e.turtleY = 100
	e.turtleAngle = 90

	result, err := e.Execute("HOME")
	if err != nil {
		t.Fatalf("Execute(HOME) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("HOME should contain turtle emoji: %q", result)
	}

	// Check turtle is at origin with angle 0
	if e.turtleX != 0 || e.turtleY != 0 {
		t.Errorf("HOME should reset position to (0,0), got (%v,%v)", e.turtleX, e.turtleY)
	}
	if e.turtleAngle != 0 {
		t.Errorf("HOME should reset angle to 0, got %v", e.turtleAngle)
	}
}

// TestExecutor_ClearScreen tests CLEARSCREEN/CS command
func TestExecutor_ClearScreen(t *testing.T) {
	e := New()
	result, err := e.Execute("CLEARSCREEN")
	if err != nil {
		t.Fatalf("Execute(CLEARSCREEN) error = %v", err)
	}
	if !strings.Contains(result, "🎨") {
		t.Errorf("CLEARSCREEN should contain graphics emoji: %q", result)
	}

	// Test short form
	result, err = e.Execute("CS")
	if err != nil {
		t.Fatalf("Execute(CS) error = %v", err)
	}
	if !strings.Contains(result, "🎨") {
		t.Errorf("CS should contain graphics emoji: %q", result)
	}
}

// TestExecutor_SetXY tests SETXY command
func TestExecutor_SetXY(t *testing.T) {
	e := New()
	result, err := e.Execute("SETXY 100 200")
	if err != nil {
		t.Fatalf("Execute(SETXY) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("SETXY should contain turtle emoji: %q", result)
	}

	if e.turtleX != 100 || e.turtleY != 200 {
		t.Errorf("SETXY should set position to (100,200), got (%v,%v)", e.turtleX, e.turtleY)
	}
}

// TestExecutor_SetHeading tests SETHEADING/SETH command
func TestExecutor_SetHeading(t *testing.T) {
	e := New()
	result, err := e.Execute("SETHEADING 45")
	if err != nil {
		t.Fatalf("Execute(SETHEADING) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("SETHEADING should contain turtle emoji: %q", result)
	}

	if e.turtleAngle != 45 {
		t.Errorf("SETHEADING should set angle to 45, got %v", e.turtleAngle)
	}

	// Test short form
	result, err = e.Execute("SETH 90")
	if err != nil {
		t.Fatalf("Execute(SETH) error = %v", err)
	}
	if !strings.Contains(result, "SETHEADING") && !strings.Contains(result, "SETH") {
		t.Errorf("SETH output missing: %q", result)
	}
	if e.turtleAngle != 90 {
		t.Errorf("SETH should set angle to 90, got %v", e.turtleAngle)
	}
}

// TestExecutor_SetColor tests SETCOLOR command
func TestExecutor_SetColor(t *testing.T) {
	e := New()
	result, err := e.Execute("SETCOLOR 255 128 64")
	if err != nil {
		t.Fatalf("Execute(SETCOLOR) error = %v", err)
	}
	if !strings.Contains(result, "🎨") {
		t.Errorf("SETCOLOR should contain graphics emoji: %q", result)
	}

	if e.penColor[0] != 255 || e.penColor[1] != 128 || e.penColor[2] != 64 {
		t.Errorf("SETCOLOR should set RGB to (255,128,64), got (%v,%v,%v)",
			e.penColor[0], e.penColor[1], e.penColor[2])
	}
}

// TestExecutor_PenWidth tests PENWIDTH command
func TestExecutor_PenWidth(t *testing.T) {
	e := New()
	result, err := e.Execute("PENWIDTH 5")
	if err != nil {
		t.Fatalf("Execute(PENWIDTH) error = %v", err)
	}
	if !strings.Contains(result, "✏️") {
		t.Errorf("PENWIDTH should contain pen emoji: %q", result)
	}

	if e.penWidth != 5 {
		t.Errorf("PENWIDTH should set width to 5, got %v", e.penWidth)
	}
}

// TestExecutor_HideShowTurtle tests HIDETURTLE/HT and SHOWTURTLE/ST commands
func TestExecutor_HideShowTurtle(t *testing.T) {
	e := New()

	// Initially turtle is visible
	if e.turtleHidden {
		t.Error("Turtle should initially be visible")
	}

	// Hide turtle
	result, err := e.Execute("HIDETURTLE")
	if err != nil {
		t.Fatalf("Execute(HIDETURTLE) error = %v", err)
	}
	if !strings.Contains(result, "🐢") {
		t.Errorf("HIDETURTLE should contain turtle emoji: %q", result)
	}
	if !e.turtleHidden {
		t.Error("HIDETURTLE should set turtleHidden to true")
	}

	// Show turtle
	result, err = e.Execute("ST")
	if err != nil {
		t.Fatalf("Execute(ST) error = %v", err)
	}
	if !strings.Contains(result, "SHOWTURTLE") {
		t.Errorf("SHOWTURTLE output missing: %q", result)
	}
	if e.turtleHidden {
		t.Error("SHOWTURTLE should set turtleHidden to false")
	}
}

// TestExecutor_Repeat tests REPEAT command
func TestExecutor_Repeat(t *testing.T) {
	e := New()
	result, err := e.Execute("REPEAT 4 [FD 50 RT 90]")
	if err != nil {
		t.Fatalf("Execute(REPEAT) error = %v", err)
	}
	// REPEAT currently returns "unsupported" message in Go stub
	if !strings.Contains(result, "🔄") {
		t.Errorf("REPEAT should contain repeat emoji: %q", result)
	}
}

// TestExecutor_UnknownCommand tests error handling for unknown Logo commands
func TestExecutor_UnknownCommand(t *testing.T) {
	e := New()
	result, err := e.Execute("FOOBAR 123")
	if err != nil {
		t.Fatalf("Execute() should not return error for unknown command: %v", err)
	}
	if !strings.Contains(result, "❌") {
		t.Errorf("Unknown command should return error message with ❌: %q", result)
	}
}

// TestExecutor_ErrorPaths covers missing arguments for movement and angle commands
func TestExecutor_ErrorPaths(t *testing.T) {
	e := New()
	cases := []string{"FORWARD", "BACK", "LEFT", "RIGHT"}
	for _, c := range cases {
		out, err := e.Execute(c)
		if err != nil {
			t.Fatalf("%s unexpected error: %v", c, err)
		}
		if !strings.Contains(out, "no") && !strings.Contains(out, "(no") {
			t.Errorf("%s missing 'no' indicator: %q", c, out)
		}
	}
}

// TestExecutor_SetXYInvalid tests invalid SETXY argument handling
func TestExecutor_SetXYInvalid(t *testing.T) {
	e := New()
	out, err := e.Execute("SETXY 100")
	if err != nil {
		t.Fatalf("SETXY invalid unexpected error: %v", err)
	}
	if !strings.Contains(out, "requires") {
		t.Errorf("SETXY invalid should show requires message: %q", out)
	}
}

// TestExecutor_SetHeadingInvalid tests invalid SETHEADING argument handling
func TestExecutor_SetHeadingInvalid(t *testing.T) {
	e := New()
	out, err := e.Execute("SETHEADING")
	if err != nil {
		t.Fatalf("SETHEADING invalid unexpected error: %v", err)
	}
	if !strings.Contains(out, "requires angle") {
		t.Errorf("SETHEADING invalid should show requires angle: %q", out)
	}
}

// TestExecutor_SetColorInvalid tests invalid SETCOLOR argument handling
func TestExecutor_SetColorInvalid(t *testing.T) {
	e := New()
	out, err := e.Execute("SETCOLOR 10 20")
	if err != nil {
		t.Fatalf("SETCOLOR invalid unexpected error: %v", err)
	}
	if !strings.Contains(out, "requires r g b") {
		t.Errorf("SETCOLOR invalid should show requires rgb: %q", out)
	}
}

// TestExecutor_PenWidthInvalid tests invalid PENWIDTH argument handling
func TestExecutor_PenWidthInvalid(t *testing.T) {
	e := New()
	out, err := e.Execute("PENWIDTH")
	if err != nil {
		t.Fatalf("PENWIDTH invalid unexpected error: %v", err)
	}
	if !strings.Contains(out, "requires width") {
		t.Errorf("PENWIDTH invalid should show requires width: %q", out)
	}
}

// TestExecutor_ToStub tests TO procedure stub branch
func TestExecutor_ToStub(t *testing.T) {
	e := New()
	out, err := e.Execute("TO FOO :X :Y")
	if err != nil {
		t.Fatalf("TO stub unexpected error: %v", err)
	}
	if !strings.Contains(out, "procedure") {
		t.Errorf("TO stub should indicate unsupported: %q", out)
	}
}

// TestExecutor_End tests END branch
func TestExecutor_End(t *testing.T) {
	e := New()
	out, err := e.Execute("END")
	if err != nil {
		t.Fatalf("END unexpected error: %v", err)
	}
	if !strings.Contains(out, "✅ END") {
		t.Errorf("END did not produce expected message: %q", out)
	}
}

// TestExecutor_ComplexSequence tests a sequence of commands
func TestExecutor_ComplexSequence(t *testing.T) {
	e := New()

	commands := []string{
		"PENUP",
		"SETXY 100 100",
		"PENDOWN",
		"FORWARD 50",
		"RIGHT 90",
		"FORWARD 50",
	}

	for i, cmd := range commands {
		result, err := e.Execute(cmd)
		if err != nil {
			t.Fatalf("Command %d (%s) error = %v", i, cmd, err)
		}
		if !strings.Contains(result, "🐢") && !strings.Contains(result, "🎨") {
			t.Errorf("Command %d (%s) should contain emoji: %q", i, cmd, result)
		}
	}

	// Verify final position is not origin
	if e.turtleX == 0 && e.turtleY == 0 {
		t.Error("After complex sequence, turtle should not be at origin")
	}
}
