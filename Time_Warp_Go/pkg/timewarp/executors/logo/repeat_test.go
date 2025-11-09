// SPDX-License-Identifier: MIT
package logo

import "testing"

func TestRepeatSquare(t *testing.T) {
	e := New()
	// draw a square 4x with FD 10 and RT 90
	out, err := e.Execute("REPEAT 4 [ FD 10 RT 90 ]")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if out == "" {
		t.Fatalf("expected output, got empty")
	}
}

func TestRepeatBadSyntax(t *testing.T) {
	e := New()
	out, _ := e.Execute("REPEAT 4 FD 10")
	if out == "" || out[:1] != "❌" {
		t.Fatalf("expected error for bad syntax, got: %q", out)
	}
}

func TestRepeatZero(t *testing.T) {
	e := New()
	out, _ := e.Execute("REPEAT 0 [ FD 10 ]")
	if out == "" { t.Fatalf("expected banner output for REPEAT 0, got empty") }
}
