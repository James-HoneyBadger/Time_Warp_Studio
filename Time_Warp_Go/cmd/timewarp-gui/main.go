// SPDX-License-Identifier: MIT
package main

import (
	"fmt"
	"image"
	"image/color"
	"image/png"
	"math"
	"strconv"
	"strings"
	"time"

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
	"github.com/James-HoneyBadger/Time_Warp/Time_Warp_Go/pkg/timewarp/executors/logo"
)

const (
	appTitle   = "Time Warp IDE (Go)"
	appVersion = "3.0.0"
)

// TurtleCanvas renders turtle graphics
type TurtleCanvas struct {
	widget.BaseWidget
	lines  []turtleLine
	x, y   float64
	angle  float64
	hidden bool
}

type turtleLine struct {
	x1, y1, x2, y2 float64
	r, g, b        uint8
	width          float64
}

func newTurtleCanvas() *TurtleCanvas {
	tc := &TurtleCanvas{
		x:     200,
		y:     200,
		angle: 90, // Up
	}
	tc.ExtendBaseWidget(tc)
	return tc
}

func (tc *TurtleCanvas) CreateRenderer() fyne.WidgetRenderer {
	return &turtleCanvasRenderer{
		canvas: tc,
		bg:     canvas.NewRectangle(color.White),
	}
}

func (tc *TurtleCanvas) Clear() {
	tc.lines = nil
	tc.x = 200
	tc.y = 200
	tc.angle = 90
	tc.hidden = false
	tc.Refresh()
}

func (tc *TurtleCanvas) AddLine(x1, y1, x2, y2 float64, r, g, b uint8, width float64) {
	tc.lines = append(tc.lines, turtleLine{x1, y1, x2, y2, r, g, b, width})
	tc.Refresh()
}

func (tc *TurtleCanvas) UpdateTurtle(x, y, angle float64) {
	tc.x = x
	tc.y = y
	tc.angle = angle
	tc.Refresh()
}

type turtleCanvasRenderer struct {
	canvas *TurtleCanvas
	bg     *canvas.Rectangle
}

func (r *turtleCanvasRenderer) Layout(size fyne.Size) {
	r.bg.Resize(size)
}

func (r *turtleCanvasRenderer) MinSize() fyne.Size {
	return fyne.NewSize(400, 400)
}

func (r *turtleCanvasRenderer) Refresh() {
	r.bg.Refresh()
	canvas.Refresh(r.canvas)
}

func (r *turtleCanvasRenderer) Objects() []fyne.CanvasObject {
	objects := []fyne.CanvasObject{r.bg}

	// Draw lines
	for _, line := range r.canvas.lines {
		l := canvas.NewLine(color.RGBA{line.r, line.g, line.b, 255})
		l.StrokeWidth = float32(line.width)
		l.Position1 = fyne.NewPos(float32(line.x1), float32(line.y1))
		l.Position2 = fyne.NewPos(float32(line.x2), float32(line.y2))
		objects = append(objects, l)
	}

	// Draw turtle indicator (skip if hidden)
	if r.canvas.hidden {
		return objects
	}
	cx := float32(r.canvas.x)
	cy := float32(r.canvas.y)
	turtleSize := float32(8.0)

	// Triangle pointing in current direction
	angleRad := (r.canvas.angle - 90) * math.Pi / 180
	dx := float32(math.Cos(angleRad) * float64(turtleSize))
	dy := float32(math.Sin(angleRad) * float64(turtleSize))

	// Simple circle for turtle
	circle := canvas.NewCircle(color.RGBA{255, 0, 0, 200})
	circle.Resize(fyne.NewSize(turtleSize*2, turtleSize*2))
	circle.Move(fyne.NewPos(cx-turtleSize, cy-turtleSize))
	objects = append(objects, circle)

	// Direction indicator
	dirLine := canvas.NewLine(color.RGBA{255, 0, 0, 255})
	dirLine.StrokeWidth = 2
	dirLine.Position1 = fyne.NewPos(cx, cy)
	dirLine.Position2 = fyne.NewPos(cx+dx, cy+dy)
	objects = append(objects, dirLine)

	return objects
}

func (r *turtleCanvasRenderer) Destroy() {}

type timeWarpGUI struct {
	app          fyne.App
	window       fyne.Window
	editor       *widget.Entry
	output       *widget.Entry
	turtleCanvas *TurtleCanvas
	// turtle state for GUI rendering
	penDown      bool
	turtleX      float64
	turtleY      float64
	turtleAngle  float64
	turtleHidden bool
	penColor     [3]uint8
	penWidth     float64
	status       *widget.Label
	turtleInfo   *widget.Label
	langSelect   *widget.Select

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
	g.turtleCanvas = newTurtleCanvas()
	g.turtleInfo = widget.NewLabel("")

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

	// Right side: turtle canvas + turtle info + output
	rightTop := container.NewBorder(nil, g.turtleInfo, nil, nil, g.turtleCanvas)
	rightSide := container.NewBorder(
		nil,
		container.NewPadded(g.output),
		nil,
		nil,
		rightTop,
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
	g.updateTurtleInfo()
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
	exportImgItem := fyne.NewMenuItem("Export Turtle Image...", g.exportTurtleImageDialog)
	exportImgItem.Icon = theme.DocumentSaveIcon()

	quitItem := fyne.NewMenuItem("Quit", func() {
		g.app.Quit()
	})

	fileMenu := fyne.NewMenu("File",
		newItem,
		fyne.NewMenuItemSeparator(),
		openItem,
		saveItem,
		saveAsItem,
		exportImgItem,
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

	// Reset turtle state
	g.penDown = true
	g.turtleX, g.turtleY, g.turtleAngle, g.turtleHidden = 0, 0, 0, false
	g.penColor = [3]uint8{0, 0, 0}
	g.penWidth = 2
	g.turtleCanvas.UpdateTurtle(200, 200, 90)
	g.updateTurtleInfo()

	// Execute line by line (interpreter processes single commands)
	// Prepare structured event capture for Logo operations
	logoExec := g.interpreter.Logo()
	logoExec.ClearEvents()
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

		// Structured events (preferred) + legacy output parsing fallback
		events := logoExec.PopEvents()
		if len(events) > 0 {
			g.applyTurtleEvents(events)
		} else {
			g.applyTurtleFromOutput(result) // fallback for non-event outputs
		}
	}

	output := outputBuilder.String()
	g.output.SetText(output)

	if strings.Contains(output, "❌") {
		g.updateStatus("Execution completed with errors")
	} else {
		g.updateStatus("✅ Execution completed")
	}
}

// applyTurtleEvents consumes structured turtle events from Logo executor.
func (g *timeWarpGUI) applyTurtleEvents(events []logo.TurtleEvent) {
	toCanvas := func(x, y float64) (float64, float64) { return 200 + x, 200 - y }
	for _, ev := range events {
		switch ev.Type {
		case "clear":
			g.turtleCanvas.Clear()
			g.turtleCanvas.UpdateTurtle(200, 200, 90)
			g.turtleX, g.turtleY, g.turtleAngle = 0, 0, 0
			g.penDown = true
		case "home":
			g.turtleX, g.turtleY, g.turtleAngle = 0, 0, 0
			g.turtleCanvas.UpdateTurtle(200, 200, 90)
		case "pen":
			g.penDown = ev.Down
		case "move":
			cx1, cy1 := toCanvas(ev.FromX, ev.FromY)
			cx2, cy2 := toCanvas(ev.ToX, ev.ToY)
			if ev.Draw {
				g.turtleCanvas.AddLine(cx1, cy1, cx2, cy2, uint8(ev.Color[0]), uint8(ev.Color[1]), uint8(ev.Color[2]), float64(ev.Width))
			}
			g.turtleX, g.turtleY = ev.ToX, ev.ToY
			g.turtleCanvas.UpdateTurtle(cx2, cy2, 90-g.turtleAngle)
		case "set_heading":
			g.turtleAngle = ev.Angle
			cx, cy := toCanvas(g.turtleX, g.turtleY)
			g.turtleCanvas.UpdateTurtle(cx, cy, 90-g.turtleAngle)
		case "set_xy":
			g.turtleX, g.turtleY = ev.ToX, ev.ToY
			cx, cy := toCanvas(ev.ToX, ev.ToY)
			g.turtleCanvas.UpdateTurtle(cx, cy, 90-g.turtleAngle)
		case "set_color":
			// Color changes reflected only on subsequent Draw events
		case "set_width":
			// Width changes reflected on subsequent Draw events
		case "show_hide":
			g.turtleHidden = ev.Hidden
			g.turtleCanvas.hidden = ev.Hidden
			g.turtleCanvas.Refresh()
		}
	}
	g.updateTurtleInfo()
}

// applyTurtleFromOutput parses Logo output lines and updates canvas
func (g *timeWarpGUI) applyTurtleFromOutput(out string) {
	u := strings.ToUpper(out)
	// Helpers
	toCanvas := func(x, y float64) (float64, float64) { // map (0,0) center to (200,200), invert Y
		return 200 + x, 200 - y
	}
	// Movement with coordinates
	if strings.HasPrefix(u, "🐢 FORWARD") || strings.HasPrefix(u, "🐢 BACK") {
		// Expect pattern: (x1,y1 → x2,y2)
		l := strings.Index(out, "(")
		r := strings.Index(out, ")")
		arrow := strings.Index(out, "→")
		if l != -1 && r != -1 && arrow != -1 && l < arrow && arrow < r {
			left := out[l+1 : arrow]
			right := out[arrow+3 : r] // skip space and arrow bytes
			parsePair := func(s string) (float64, float64, bool) {
				parts := strings.Split(s, ",")
				if len(parts) != 2 {
					return 0, 0, false
				}
				x, err1 := strconv.ParseFloat(strings.TrimSpace(parts[0]), 64)
				y, err2 := strconv.ParseFloat(strings.TrimSpace(parts[1]), 64)
				return x, y, err1 == nil && err2 == nil
			}
			x1, y1, ok1 := parsePair(left)
			x2, y2, ok2 := parsePair(right)
			if ok1 && ok2 {
				cx1, cy1 := toCanvas(x1, y1)
				cx2, cy2 := toCanvas(x2, y2)
				if g.penDown {
					g.turtleCanvas.AddLine(cx1, cy1, cx2, cy2, g.penColor[0], g.penColor[1], g.penColor[2], g.penWidth)
				}
				g.turtleX, g.turtleY = x2, y2
				// Angle isn't included here; leave as-is
				// Update turtle marker (convert to canvas orientation where 90 is up)
				g.turtleCanvas.UpdateTurtle(cx2, cy2, 90-g.turtleAngle)
				g.updateTurtleInfo()
				return
			}
		}
	}
	// Heading updates
	if strings.HasPrefix(u, "🐢 LEFT") || strings.HasPrefix(u, "🐢 RIGHT") || strings.HasPrefix(u, "🐢 SETHEADING") {
		// Try to find "heading: X°" or after command
		headingIdx := strings.Index(u, "HEADING:")
		if headingIdx != -1 {
			rest := u[headingIdx+8:]
			fields := strings.Fields(rest)
			if len(fields) > 0 {
				v := strings.TrimSuffix(fields[0], "°")
				if ang, err := strconv.ParseFloat(v, 64); err == nil {
					g.turtleAngle = ang
				}
			}
		} else if strings.HasPrefix(u, "🐢 SETHEADING") {
			// Format: 🐢 SETHEADING X°
			parts := strings.Fields(u)
			if len(parts) >= 3 {
				v := strings.TrimSuffix(parts[2], "°")
				if ang, err := strconv.ParseFloat(v, 64); err == nil {
					g.turtleAngle = ang
				}
			}
		}
		cx, cy := toCanvas(g.turtleX, g.turtleY)
		g.turtleCanvas.UpdateTurtle(cx, cy, 90-g.turtleAngle)
		g.updateTurtleInfo()
		return
	}
	// Pen state
	// Pen attributes
	if strings.HasPrefix(u, "🎨 SETCOLOR") {
		parts := strings.Fields(u)
		if len(parts) >= 5 { // 🎨 SETCOLOR r g b
			rVal, _ := strconv.Atoi(parts[2])
			gVal, _ := strconv.Atoi(parts[3])
			bVal, _ := strconv.Atoi(parts[4])
			g.penColor = [3]uint8{uint8(rVal), uint8(gVal), uint8(bVal)}
		}
		return
	}
	if strings.HasPrefix(u, "✏️  PENWIDTH") { // note space after emoji
		parts := strings.Fields(u)
		if len(parts) >= 3 {
			if w, err := strconv.ParseFloat(parts[2], 64); err == nil {
				g.penWidth = w
			}
		}
		return
	}
	if strings.HasPrefix(u, "🐢 PENUP") {
		g.penDown = false
		return
	}
	if strings.HasPrefix(u, "🐢 PENDOWN") {
		g.penDown = true
		return
	}
	// Turtle visibility
	if strings.HasPrefix(u, "🐢 HIDETURTLE") {
		g.turtleHidden = true
		g.turtleCanvas.hidden = true
		g.turtleCanvas.Refresh()
		g.updateTurtleInfo()
		return
	}
	if strings.HasPrefix(u, "🐢 SHOWTURTLE") {
		g.turtleHidden = false
		g.turtleCanvas.hidden = false
		g.turtleCanvas.Refresh()
		g.updateTurtleInfo()
		return
	}
	// Clear/Home
	if strings.HasPrefix(u, "🎨 SCREEN CLEARED") || strings.HasPrefix(u, "🐢 HOME") {
		g.turtleX, g.turtleY, g.turtleAngle = 0, 0, 0
		g.turtleCanvas.Clear()
		g.turtleCanvas.UpdateTurtle(200, 200, 90)
		g.updateTurtleInfo()
		return
	}
	// SETXY
	if strings.HasPrefix(u, "🐢 SETXY") {
		// Format: 🐢 SETXY x y (from ...)
		parts := strings.Fields(out)
		if len(parts) >= 4 {
			if x, err1 := strconv.ParseFloat(parts[2], 64); err1 == nil {
				if y, err2 := strconv.ParseFloat(parts[3], 64); err2 == nil {
					g.turtleX, g.turtleY = x, y
					cx, cy := toCanvas(x, y)
					g.turtleCanvas.UpdateTurtle(cx, cy, 90-g.turtleAngle)
					g.updateTurtleInfo()
				}
			}
		}
		return
	}
}

// updateTurtleInfo refreshes the small info label under the canvas
func (g *timeWarpGUI) updateTurtleInfo() {
	if g.turtleInfo == nil {
		return
	}
	vis := "shown"
	if g.turtleHidden {
		vis = "hidden"
	}
	pen := "up"
	if g.penDown {
		pen = "down"
	}
	g.turtleInfo.SetText(
		fmt.Sprintf(
			"Turtle: (%.1f, %.1f) heading %.1f°, pen %s, color %d,%d,%d, width %.1f, %s",
			g.turtleX, g.turtleY, g.turtleAngle, pen, g.penColor[0], g.penColor[1], g.penColor[2], g.penWidth, vis,
		),
	)
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
	g.turtleCanvas.Clear()
	g.updateStatus("Output and canvas cleared")
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

// exportTurtleImageDialog prompts for export options then saves PNG.
func (g *timeWarpGUI) exportTurtleImageDialog() {
	if g.turtleCanvas == nil { return }
	wField := widget.NewEntry(); wField.SetText("800")
	hField := widget.NewEntry(); hField.SetText("800")
	preset := widget.NewSelect([]string{"Custom","400x400","800x800","1024x1024","1600x1600"}, func(s string){
		switch s {
		case "400x400": wField.SetText("400"); hField.SetText("400")
		case "800x800": wField.SetText("800"); hField.SetText("800")
		case "1024x1024": wField.SetText("1024"); hField.SetText("1024")
		case "1600x1600": wField.SetText("1600"); hField.SetText("1600")
		}
	})
	preset.SetSelected("Custom")
	transparent := widget.NewCheck("Transparent background", nil)
	selectedBg := color.RGBA{255,255,255,255}
	bgButton := widget.NewButton("Background color (#FFFFFF)", func(){
		cp := dialog.NewColorPicker("Pick background color", "Choose a color", func(c color.Color){
			n := color.NRGBAModel.Convert(c).(color.NRGBA)
			selectedBg = color.RGBA{n.R, n.G, n.B, 255}
			bgButton.SetText(fmt.Sprintf("Background color (%s)", formatHex(selectedBg)))
		}, g.window)
		cp.Advanced = true
		cp.Show()
	})
	bgButton.Enable()
	transparent.OnChanged = func(b bool){ if b { bgButton.Disable() } else { bgButton.Enable() } }
	includeTurtle := widget.NewCheck("Include turtle indicator", nil); includeTurtle.SetChecked(true)
	items := []*widget.FormItem{
		{Text: "Preset", Widget: preset},
		{Text: "Width", Widget: wField},
		{Text: "Height", Widget: hField},
		{Text: "", Widget: transparent},
		{Text: "", Widget: bgButton},
		{Text: "", Widget: includeTurtle},
	}
	dialog.ShowForm("Export Turtle Image", "Export", "Cancel", items, func(ok bool) {
		if !ok { return }
		const maxSize = 8192
		wVal, wErr := strconv.Atoi(strings.TrimSpace(wField.Text))
		hVal, hErr := strconv.Atoi(strings.TrimSpace(hField.Text))
		if wErr != nil || hErr != nil || wVal <= 0 || hVal <= 0 || wVal > maxSize || hVal > maxSize {
			dialog.ShowError(fmt.Errorf("invalid size: enter 1-%d for width/height", maxSize), g.window)
			g.exportTurtleImageDialog()
			return
		}
		var bg color.RGBA
		if transparent.Checked { bg = color.RGBA{0,0,0,0} } else { bg = selectedBg }
		g.performTurtleExport(wVal, hVal, bg, includeTurtle.Checked)
	}, g.window)
}

// performTurtleExport generates PNG with options and file save dialog.
func (g *timeWarpGUI) performTurtleExport(width, height int, bg color.RGBA, drawTurtle bool) {
	img := image.NewRGBA(image.Rect(0, 0, width, height))
	for y := 0; y < height; y++ { for x := 0; x < width; x++ { img.Set(x, y, bg) } }
	scaleX := float64(width) / 400.0
	scaleY := float64(height) / 400.0
	wScale := (scaleX + scaleY) / 2.0
	drawLine := func(x1, y1, x2, y2 int, col color.RGBA, w int) {
		dx := int(math.Abs(float64(x2 - x1)))
		dy := -int(math.Abs(float64(y2 - y1)))
		sx := -1; if x1 < x2 { sx = 1 }
		sy := -1; if y1 < y2 { sy = 1 }
		err := dx + dy; x, y := x1, y1
		for {
			for oy := -w/2; oy <= w/2; oy++ { for ox := -w/2; ox <= w/2; ox++ { px := x+ox; py := y+oy; if px>=0 && px<width && py>=0 && py<height { img.Set(px,py,col) } } }
			if x == x2 && y == y2 { break }
			e2 := 2*err; if e2 >= dy { err += dy; x += sx }; if e2 <= dx { err += dx; y += sy }
		}
	}
	// draw lines with clamped width
	for _, ln := range g.turtleCanvas.lines {
		x1 := int(ln.x1 * scaleX); y1 := int(ln.y1 * scaleY); x2 := int(ln.x2 * scaleX); y2 := int(ln.y2 * scaleY)
		sw := int(math.Round(ln.width * wScale)); if sw < 1 { sw = 1 }; if sw > 20 { sw = 20 }
		drawLine(x1, y1, x2, y2, color.RGBA{ln.r, ln.g, ln.b, 255}, sw)
	}
	// optional turtle indicator
	if drawTurtle && !g.turtleHidden {
		tx := int(g.turtleCanvas.x * scaleX); ty := int(g.turtleCanvas.y * scaleY)
		radius := int(math.Round(8 * wScale)); if radius < 3 { radius = 3 }
		for ay := -radius; ay <= radius; ay++ { for ax := -radius; ax <= radius; ax++ { if ax*ax+ay*ay <= radius*radius { px:=tx+ax; py:=ty+ay; if px>=0 && px<width && py>=0 && py<height { img.Set(px,py,color.RGBA{255,0,0,220}) } } } }
		angleRad := (g.turtleCanvas.angle - 90) * math.Pi / 180
		lx := tx + int(math.Cos(angleRad)*float64(radius)); ly := ty + int(math.Sin(angleRad)*float64(radius))
		headW := int(math.Round(wScale)); if headW < 1 { headW = 1 }; if headW > 6 { headW = 6 }
		drawLine(tx, ty, lx, ly, color.RGBA{255, 0, 0, 255}, headW)
	}
	dialog.ShowFileSave(func(writer fyne.URIWriteCloser, err error) {
		if err != nil || writer == nil { return }
		defer writer.Close(); if err := png.Encode(writer, img); err != nil { dialog.ShowError(err, g.window); return }
		g.updateStatus(fmt.Sprintf("Exported %dx%d turtle image: %s", width, height, writer.URI().Name()))
	}, g.window)
	go func() {
		name := fmt.Sprintf("turtle_%s_%dx%d.png", time.Now().Format("20060102_150405"), width, height)
		uri := storage.NewFileURI("/tmp/" + name)
		w, err := storage.Writer(uri); if err == nil { _ = png.Encode(w, img); w.Close() }
	}()
}

// parseHexColor parses #RRGGBB into color.RGBA (opaque)
func parseHexColor(s string) (color.RGBA, error) {
	s = strings.TrimSpace(s)
	var c color.RGBA
	if len(s) == 7 && strings.HasPrefix(s, "#") {
		r, rErr := strconv.ParseUint(s[1:3], 16, 8)
		g, gErr := strconv.ParseUint(s[3:5], 16, 8)
		b, bErr := strconv.ParseUint(s[5:7], 16, 8)
		if rErr == nil && gErr == nil && bErr == nil {
			c = color.RGBA{uint8(r), uint8(g), uint8(b), 255}
			return c, nil
		}
	}
	return c, fmt.Errorf("invalid hex color")
}
