package timewarp

import "testing"

func TestInterpreterBasicEcho(t *testing.T) {
	interp := NewInterpreter()
	out := interp.Execute("ECHO Hello World")
	if out != "✅ Hello World\n" {
		t.Fatalf("unexpected output: %q", out)
	}
}
