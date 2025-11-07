package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp"
)

// Simple CLI entry point for the Go version of Time Warp.
// Reads commands from stdin and prints interpreter output.
func main() {
	interp := timewarp.NewInterpreter()

	if len(os.Args) > 1 {
		// Execute a single command passed via args
		cmd := strings.Join(os.Args[1:], " ")
		out := interp.Execute(cmd)
		fmt.Print(out)
		return
	}

	scanner := bufio.NewScanner(os.Stdin)
	fmt.Println("ℹ️  Time Warp (Go) — enter commands (Ctrl+D to exit)")
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			break
		}
		line := scanner.Text()
		if strings.TrimSpace(line) == "" {
			continue
		}
		out := interp.Execute(line)
		fmt.Print(out)
	}
}
