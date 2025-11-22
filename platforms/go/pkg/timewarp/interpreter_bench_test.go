// SPDX-License-Identifier: MIT
package timewarp

import (
	"strings"
	"testing"
)

func BenchmarkExecuteBasicLoop(b *testing.B) {
	interp := NewInterpreter()
	program := strings.Join([]string{
		"10 S = 0",
		"20 FOR I = 1 TO 100",
		"30 S = S + I",
		"40 NEXT I",
		"50 PRINT S",
		"60 END",
	}, "\n")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = interp.Execute(program)
	}
}

func BenchmarkLogoSquare(b *testing.B) {
	interp := NewInterpreter()
	code := []string{
		"CS",
		"REPEAT 4", // unsupported loop in Go stub; expand manually
		"FD 100",
		"RT 90",
	}
	// expand to 4 sides
	expanded := make([]string, 0, 8)
	for i := 0; i < 4; i++ {
		expanded = append(expanded, code[0+1:]...)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, line := range expanded {
			_ = interp.Execute(line)
		}
	}
}
