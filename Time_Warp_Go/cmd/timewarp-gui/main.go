package main

import (
	"fmt"
	"image/color"
	"strings"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/dialog"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/storage"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"

	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp"
)

const (
	appTitle   = "Time Warp IDE (Go)"
	appVersion = "3.0.0"
)

type timeWarpGUI struct {
	app        fyne.App
	window     fyne.Window
	editor     *widget.Entry
	output     *widget.Entry
	canvas     *fyne.Container
	status     *widget.Label
	langSelect *widget.Select

	interpreter *timewarp.Interpreter
	currentFile fyne.URI
}

func main() {
	gui := newTimeWarpGUI()
	gui.show()
}

func newTimeWarpGUI() *timeWarpGUI {
	a := app.NewWithID("org.honeybadger.timewarp-go")
	w := a.NewWindow(appTitle + " v" + appVersion)
	w.Resize(fyne.NewSize(1000, 700))

	gui := &timeWarpGUI{
		app:    a,
		window: w,
	}

	// Initialize interpreter
	gui.interpreter = timewarp.NewInterpreter()

	gui.buildUI()
	return gui
}

func (g *timeWarpGUI) buildUI() {
	// Menu bar
	g.buildMenus()

	// Editor (left side)
	g.editor = widget.NewMultiLineEntry()
	g.editor.SetPlaceHolder("Enter your TempleCode program here...\n\nExamples:\n  PRINT \"Hello World\"\n  FOR i = 1 TO 10\n    PRINT i\n  NEXT i")
	g.editor.Wrapping = fyne.TextWrapWord

	// Output (bottom right)
	g.output = widget.NewMultiLineEntry()
	g.output.SetPlaceHolder("Program output will appear here...")
	g.output.Disable()
	g.output.Wrapping = fyne.TextWrapWord

	// Turtle canvas (top right)
	g.canvas = container.NewStack(
		canvas.NewRectangle(color.White),
		canvas.NewText("Turtle Graphics Canvas\n(Logo commands will draw here)", color.Black),
	)

	// Status bar (initialize early before widgets that use it)
	g.status = widget.NewLabel("Ready")

	// Language selector
	g.langSelect = widget.NewSelect([]string{"Auto", "BASIC", "PILOT", "LOGO"}, func(value string) {
		g.updateStatus("Language: " + value)
	})
	g.langSelect.SetSelected("Auto")

	// Buttons
	runBtn := widget.NewButtonWithIcon("Run", theme.MediaPlayIcon(), g.runProgram)
	runBtn.Importance = widget.HighImportance

	clearBtn := widget.NewButtonWithIcon("Clear Output", theme.ContentClearIcon(), g.clearOutput)
	newBtn := widget.NewButtonWithIcon("New", theme.DocumentCreateIcon(), g.newFile)
	openBtn := widget.NewButtonWithIcon("Open", theme.FolderOpenIcon(), g.openFile)
	saveBtn := widget.NewButtonWithIcon("Save", theme.DocumentSaveIcon(), g.saveFile)

	toolbar := container.NewHBox(
		newBtn,
		openBtn,
		saveBtn,
		layout.NewSpacer(),
		widget.NewLabel("Language:"),
		g.langSelect,
		layout.NewSpacer(),
		runBtn,
		clearBtn,
	)

	// Status bar already initialized earlier
	statusBar := container.NewBorder(nil, nil, nil, nil, g.status)

	// Right side: canvas + output
	rightSide := container.NewBorder(
		nil,
		container.NewPadded(g.output),
		nil,
		nil,
		g.canvas,
	)

	// Main split
	split := container.NewHSplit(
		container.NewBorder(nil, nil, nil, nil, g.editor),
		rightSide,
	)
	split.SetOffset(0.5)

	// Main layout
	content := container.NewBorder(
		toolbar,
		statusBar,
		nil,
		nil,
		split,
	)

	g.window.SetContent(content)
	g.updateStatus("Ready - Select language or use Auto-detect")
}

func (g *timeWarpGUI) buildMenus() {
	// File menu
	newItem := fyne.NewMenuItem("New", g.newFile)
	newItem.Icon = theme.DocumentCreateIcon()

	openItem := fyne.NewMenuItem("Open...", g.openFile)
	openItem.Icon = theme.FolderOpenIcon()

	saveItem := fyne.NewMenuItem("Save", g.saveFile)
	saveItem.Icon = theme.DocumentSaveIcon()

	saveAsItem := fyne.NewMenuItem("Save As...", g.saveFileAs)

	quitItem := fyne.NewMenuItem("Quit", func() {
		g.app.Quit()
	})

	fileMenu := fyne.NewMenu("File",
		newItem,
		fyne.NewMenuItemSeparator(),
		openItem,
		saveItem,
		saveAsItem,
		fyne.NewMenuItemSeparator(),
		quitItem,
	)

	// Run menu
	runItem := fyne.NewMenuItem("Run Program", g.runProgram)
	runItem.Icon = theme.MediaPlayIcon()

	clearItem := fyne.NewMenuItem("Clear Output", g.clearOutput)
	clearItem.Icon = theme.ContentClearIcon()

	runMenu := fyne.NewMenu("Run",
		runItem,
		fyne.NewMenuItemSeparator(),
		clearItem,
	)

	// Help menu
	aboutItem := fyne.NewMenuItem("About", g.showAbout)
	aboutItem.Icon = theme.InfoIcon()

	helpMenu := fyne.NewMenu("Help", aboutItem)

	mainMenu := fyne.NewMainMenu(fileMenu, runMenu, helpMenu)
	g.window.SetMainMenu(mainMenu)
}

func (g *timeWarpGUI) runProgram() {
	code := g.editor.Text
	if strings.TrimSpace(code) == "" {
		g.updateStatus("No code to run")
		dialog.ShowInformation("No Code", "Please enter some code to run.", g.window)
		return
	}

	g.clearOutput()
	g.updateStatus("Running...")

	// Determine language
	lang := g.langSelect.Selected
	if lang == "Auto" {
		lang = g.detectLanguage(code)
		g.updateStatus(fmt.Sprintf("Running as %s...", lang))
	}

	// Execute line by line (interpreter processes single commands)
	var outputBuilder strings.Builder
	lines := strings.Split(code, "\n")

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "REM") || strings.HasPrefix(line, "#") {
			continue
		}

		result := g.interpreter.Execute(line)
		outputBuilder.WriteString(result)
		outputBuilder.WriteString("\n")
	}

	output := outputBuilder.String()
	g.output.SetText(output)

	if strings.Contains(output, "❌") {
		g.updateStatus("Execution completed with errors")
	} else {
		g.updateStatus("✅ Execution completed")
	}
}

func (g *timeWarpGUI) detectLanguage(code string) string {
	lines := strings.Split(code, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "REM") || strings.HasPrefix(line, "#") {
			continue
		}

		// PILOT detection (T:, A:, etc.)
		if len(line) >= 2 && line[1] == ':' {
			return "PILOT"
		}

		// Logo detection (common commands)
		upper := strings.ToUpper(line)
		if strings.HasPrefix(upper, "FD ") || strings.HasPrefix(upper, "FORWARD ") ||
			strings.HasPrefix(upper, "LT ") || strings.HasPrefix(upper, "LEFT ") ||
			strings.HasPrefix(upper, "RT ") || strings.HasPrefix(upper, "RIGHT ") {
			return "LOGO"
		}

		// Default to BASIC
		return "BASIC"
	}
	return "BASIC"
}

func (g *timeWarpGUI) clearOutput() {
	g.output.SetText("")
	g.updateStatus("Output cleared")
}

func (g *timeWarpGUI) newFile() {
	if g.editor.Text != "" {
		dialog.ShowConfirm("New File", "Discard current file?", func(ok bool) {
			if ok {
				g.editor.SetText("")
				g.currentFile = nil
				g.updateStatus("New file")
			}
		}, g.window)
	} else {
		g.editor.SetText("")
		g.currentFile = nil
		g.updateStatus("New file")
	}
}

func (g *timeWarpGUI) openFile() {
	dialog.ShowFileOpen(func(reader fyne.URIReadCloser, err error) {
		if err != nil || reader == nil {
			return
		}
		defer reader.Close()

		data := make([]byte, 1024*1024) // 1MB max
		n, _ := reader.Read(data)

		g.editor.SetText(string(data[:n]))
		g.currentFile = reader.URI()
		g.updateStatus("Opened: " + reader.URI().Name())
	}, g.window)
}

func (g *timeWarpGUI) saveFile() {
	if g.currentFile != nil {
		g.doSave(g.currentFile)
	} else {
		g.saveFileAs()
	}
}

func (g *timeWarpGUI) saveFileAs() {
	dialog.ShowFileSave(func(writer fyne.URIWriteCloser, err error) {
		if err != nil || writer == nil {
			return
		}
		defer writer.Close()

		_, _ = writer.Write([]byte(g.editor.Text))
		g.currentFile = writer.URI()
		g.updateStatus("Saved: " + writer.URI().Name())
	}, g.window)
}

func (g *timeWarpGUI) doSave(uri fyne.URI) {
	writer, err := storage.Writer(uri)
	if err != nil {
		dialog.ShowError(err, g.window)
		return
	}
	defer writer.Close()

	_, _ = writer.Write([]byte(g.editor.Text))
	g.updateStatus("Saved: " + uri.Name())
}

func (g *timeWarpGUI) showAbout() {
	about := fmt.Sprintf(`Time Warp IDE (Go Edition)
Version %s

A cross-platform interpreter for TempleCode
(BASIC + PILOT + Logo)

Built with Fyne GUI toolkit
© 2025 James Temple`, appVersion)

	dialog.ShowCustom("About", "Close", widget.NewLabel(about), g.window)
}

func (g *timeWarpGUI) updateStatus(msg string) {
	g.status.SetText(msg)
}

func (g *timeWarpGUI) show() {
	g.window.ShowAndRun()
}
