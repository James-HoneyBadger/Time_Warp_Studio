package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp"
	basexec "github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp/executors/basic"
	pilotexec "github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp/executors/pilot"
)

// Simple CLI entry point for the Go version of Time Warp.
// Reads commands from stdin and prints interpreter output.
func main() {
	// Batch mode: `timewarp --batch BASIC` reads program from stdin and executes
	if len(os.Args) >= 3 && (os.Args[1] == "--batch" || os.Args[1] == "-b") {
		lang := strings.ToUpper(os.Args[2])
		data, _ := io.ReadAll(os.Stdin)
		program := string(data)
		switch lang {
		case "BASIC":
			exec := basexec.New()
			out := exec.RunProgram(program)
			fmt.Print(out)
			return
		case "PILOT":
			exec := pilotexec.New()
			out := exec.RunProgram(program)
			fmt.Print(out)
			return
		default:
			fmt.Printf("❌ Batch mode unsupported for %s\n", lang)
			return
		}
	}

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
