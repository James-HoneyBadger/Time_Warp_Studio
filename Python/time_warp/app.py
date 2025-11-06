"""Time Warp IDE (Tkinter) application for TempleCode.

This module provides a simple GUI editor, console, and canvas backed by a
TempleCode interpreter. It includes syntax highlighting, examples, and small
utilities for DB testing. The file is intentionally large; we disable the
corresponding linter warning for module length.
"""

# pylint: disable=too-many-lines

import threading
import tkinter as tk
from tkinter import ttk, filedialog, messagebox, simpledialog
import math
import re
from typing import Optional, cast
import os
import json

from templecode import TempleInterpreter, IOBase, TurtleAPI

# Precompiled regex patterns and keyword list for syntax highlighting
_STRING_PATTERN = re.compile(
    "|".join(
        [
            r'"[^"\\]*(?:\\.[^"\\]*)*"',
            r"'[^'\\]*(?:\\.[^'\\]*)*'",
        ]
    )
)
_KEYWORDS = [
    # control & flow
    "PRINT",
    "TYPE",
    "INPUT",
    "ACCEPT",
    "LET",
    "IF",
    "THEN",
    "ELSE",
    "GOTO",
    "JUMP",
    "FOR",
    "NEXT",
    "WHILE",
    "ENDWHILE",
    "REPEAT",
    "ENDREPEAT",
    "SLEEP",
    "PROC",
    "ENDPROC",
    "CALL",
    "RETURN",
    "ASSERT",
    "DUMPVARS",
    "TRACE",
    "PAUSE",
    # turtle
    "FD",
    "FORWARD",
    "LT",
    "LEFT",
    "RT",
    "RIGHT",
    "PU",
    "PENUP",
    "PD",
    "PENDOWN",
    "CLS",
    "CLEAR",
    "SETXY",
    "COLOR",
    "PENWIDTH",
    "FILLCOLOR",
    "BACKGROUND",
    "TEXT",
    "RECT",
    "CIRCLE",
    "HOME",
    "SETHEADING",
    # file/csv
    "READFILE",
    "WRITEFILE",
    "CSVREAD",
    "CSVWRITE",
    "APPENDFILE",
    "EXISTS",
    "DIRLIST",
    "READJSON",
    "WRITEJSON",
    # db
    "INTO",
    "DBCONNECT",
    "DBDISCONNECT",
    "DBEXEC",
    "DBEXECPARAM",
    "DBQUERY",
    "DBQUERYPARAM",
]
_KW_PATTERN = re.compile(
    r"\b(" + "|".join(map(re.escape, _KEYWORDS)) + r")\b",
    re.IGNORECASE,
)


class TextIO(IOBase):
    """Thread-safe Text-based I/O adapter bound to a Tk Text widget."""

    def __init__(self, text_widget: tk.Text, root: tk.Tk):
        self.text = text_widget
        self.root = root

    def _call_on_ui(self, func, *args, **kwargs):
        # Run the given func on the Tk main thread and wait for completion
        done = threading.Event()
        result = {"value": None}

        def wrapper():
            try:
                result["value"] = func(*args, **kwargs)
            finally:
                done.set()

        self.root.after(0, wrapper)
        done.wait()
        return result["value"]

    def write(self, text: str) -> None:
        def _do_write():
            self.text.configure(state="normal")
            self.text.insert("end", text)
            self.text.see("end")
            self.text.configure(state="disabled")
            self.text.update_idletasks()

        self._call_on_ui(_do_write)

    def read(self, prompt: str = "") -> str:
        if prompt:
            self.write(prompt)

        def _ask():
            return simpledialog.askstring(
                "Input",
                prompt or "Input:",
                parent=self.root,
            )

        value = self._call_on_ui(_ask) or ""
        # Echo the user's input to the console to mirror terminal behavior
        self.write(value + "\n")
        return value


class CanvasTurtle(TurtleAPI):
    """Minimal turtle-like drawing API implemented on a Tk Canvas."""

    def __init__(self, canvas: tk.Canvas):
        self.canvas = canvas
        self.width = int(canvas.cget("width"))
        self.height = int(canvas.cget("height"))
        self.x = self.width / 2
        self.y = self.height / 2
        self.heading = 0.0  # degrees, 0 to the right
        self.pendown_state = True
        self.color_rgb = (0, 0, 0)
        self.fill_rgb = (200, 200, 200)
        self.pen_width = 1.0

    def _call_on_ui(self, func, *args, **kwargs):
        done = threading.Event()

        def wrapper():
            try:
                func(*args, **kwargs)
            finally:
                done.set()

        self.canvas.after(0, wrapper)
        done.wait()

    def clear(self) -> None:
        def _do_clear():
            self.canvas.delete("all")

        self._call_on_ui(_do_clear)
        self.x = self.width / 2
        self.y = self.height / 2
        self.heading = 0.0

    def _color_hex(self) -> str:
        r, g, b = self.color_rgb
        return f"#{r:02x}{g:02x}{b:02x}"

    def forward(self, n: float) -> None:
        rad = math.radians(self.heading)
        nx = self.x + n * math.cos(rad)
        ny = self.y - n * math.sin(rad)
        if self.pendown_state:

            def _do_line():
                self.canvas.create_line(
                    self.x,
                    self.y,
                    nx,
                    ny,
                    fill=self._color_hex(),
                    width=self.pen_width,
                )

            self._call_on_ui(_do_line)
        self.x, self.y = nx, ny

    def left(self, a: float) -> None:
        self.heading = (self.heading + a) % 360

    def right(self, a: float) -> None:
        self.heading = (self.heading - a) % 360

    def penup(self) -> None:
        self.pendown_state = False

    def pendown(self) -> None:
        self.pendown_state = True

    def setxy(self, x: float, y: float) -> None:
        # If pen is down, draw a line from current position to (x, y)
        if self.pendown_state:

            def _do_line():
                self.canvas.create_line(
                    self.x,
                    self.y,
                    x,
                    y,
                    fill=self._color_hex(),
                    width=self.pen_width,
                )

            self._call_on_ui(_do_line)
        self.x = x
        self.y = y

    def color(self, r: int, g: int, b: int) -> None:
        self.color_rgb = (
            max(0, min(255, int(r))),
            max(0, min(255, int(g))),
            max(0, min(255, int(b))),
        )

    # Extended drawing API
    def penwidth(self, w: float) -> None:
        self.pen_width = max(0.5, float(w))

    def fillcolor(self, r: int, g: int, b: int) -> None:
        self.fill_rgb = (
            max(0, min(255, int(r))),
            max(0, min(255, int(g))),
            max(0, min(255, int(b))),
        )

    def _fill_hex(self) -> str:
        r, g, b = self.fill_rgb
        return f"#{r:02x}{g:02x}{b:02x}"

    def background(self, r: int, g: int, b: int) -> None:
        rr = max(0, min(255, int(r)))
        gg = max(0, min(255, int(g)))
        bb = max(0, min(255, int(b)))
        bg = f"#{rr:02x}{gg:02x}{bb:02x}"

        def _do_bg():
            self.canvas.configure(bg=bg)

        self._call_on_ui(_do_bg)

    def text(self, x: float, y: float, s: str) -> None:
        def _do_text():
            self.canvas.create_text(
                x,
                y,
                text=s,
                fill=self._color_hex(),
                anchor="nw",
            )

        self._call_on_ui(_do_text)

    def rect(
        self,
        x: float,
        y: float,
        w: float,
        h: float,
        fill: bool = False,
    ) -> None:
        outline = self._color_hex()
        fillc = self._fill_hex() if fill else ""

        def _do_rect():
            self.canvas.create_rectangle(
                x,
                y,
                x + w,
                y + h,
                outline=outline,
                fill=fillc,
                width=self.pen_width,
            )

        self._call_on_ui(_do_rect)

    def circle(self, x: float, y: float, r: float, fill: bool = False) -> None:
        outline = self._color_hex()
        fillc = self._fill_hex() if fill else ""

        def _do_oval():
            self.canvas.create_oval(
                x - r,
                y - r,
                x + r,
                y + r,
                outline=outline,
                fill=fillc,
                width=self.pen_width,
            )

        self._call_on_ui(_do_oval)

    def home(self) -> None:
        self.x = self.width / 2
        self.y = self.height / 2

    def setheading(self, a: float) -> None:
        self.heading = float(a) % 360


class TimeWarpApp(ttk.Frame):
    """Main Time Warp IDE window with editor, canvas, and console."""

    def __init__(self, master: Optional[tk.Misc] = None):
        super().__init__(master)
        self.pack(fill="both", expand=True)
        self.root = cast(tk.Tk, self.winfo_toplevel())
        self.root.title("Time Warp v3.0.0")
        self.root.protocol("WM_DELETE_WINDOW", self._on_close)

        # Settings (theme, fonts, geometry, db defaults)
        self.settings = {
            "theme": "light",
            "editor_font_size": 11,
            "console_font_size": 11,
            "geometry": None,
            "show_line_numbers": True,
            # Default DB connection (used by Tools > Insert MySQL Boilerplate)
            "db_host": "localhost",
            "db_user": "root",
            "db_password": "",
            "db_name": "test",
            "db_port": 3306,
        }

        # Debugging toggle variable must exist before building menus
        self.trace_var = tk.BooleanVar(value=False)
        self.wrap_var = tk.BooleanVar(value=False)
        self.show_line_numbers_var = tk.BooleanVar(value=True)
        self.theme_var = tk.StringVar(value="light")

        self._build_ui()

        # Load and apply saved settings
        self._load_settings()
        self._apply_settings()

        self.stop_event = threading.Event()
        self.interpreter: Optional[TempleInterpreter] = None
        self.runner_thread: Optional[threading.Thread] = None
        self.current_file: Optional[str] = None

        self._load_default_example()
        self._update_status()
        self._update_title()

        # Keyboard shortcuts
        self.root.bind("<Control-n>", lambda e: self._new_file())
        self.root.bind("<Control-o>", lambda e: self._open_file())
        self.root.bind("<Control-s>", lambda e: self._save_file())
        self.root.bind("<Control-Shift-S>", lambda e: self._save_file_as())
        self.root.bind("<F5>", lambda e: self._run())
        self.root.bind("<Escape>", lambda e: self._stop())
        self.root.bind("<Control-f>", lambda e: self._find_dialog())
        self.root.bind("<Control-h>", lambda e: self._replace_dialog())
        self.root.bind("<Control-l>", lambda e: self._goto_line())
        self.root.bind("<Control-slash>", lambda e: self._toggle_comment())
        self.root.bind("<Control-d>", lambda e: self._duplicate_line())
        # Debug insert shortcuts
        self.root.bind("<Control-Shift-T>", lambda e: self._insert_trace())
        self.root.bind("<Control-Shift-V>", lambda e: self._insert_dumpvars())
        self.root.bind("<Control-Shift-P>", lambda e: self._insert_pause())

    def _build_ui(self) -> None:
        self.root.geometry("1100x700")
        self.root.minsize(900, 600)

        # Menu bar
        menubar = tk.Menu(self.root)
        self.root.config(menu=menubar)

        # File menu
        file_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="File", menu=file_menu)
        file_menu.add_command(
            label="New",
            command=self._new_file,
            accelerator="Ctrl+N",
        )
        file_menu.add_command(
            label="Open...", command=self._open_file, accelerator="Ctrl+O"
        )
        file_menu.add_command(
            label="Save", command=self._save_file, accelerator="Ctrl+S"
        )
        file_menu.add_command(
            label="Save As...",
            command=self._save_file_as,
            accelerator="Ctrl+Shift+S",
        )
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self.root.quit)

        # Edit menu
        edit_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Edit", menu=edit_menu)
        edit_menu.add_command(
            label="Undo",
            command=self._edit_undo,
        )
        edit_menu.add_command(
            label="Redo",
            command=self._edit_redo,
        )
        edit_menu.add_separator()
        edit_menu.add_command(
            label="Cut",
            command=lambda: self.editor.event_generate("<<Cut>>"),
        )
        edit_menu.add_command(
            label="Copy",
            command=lambda: self.editor.event_generate("<<Copy>>"),
        )
        edit_menu.add_command(
            label="Paste",
            command=lambda: self.editor.event_generate("<<Paste>>"),
        )
        edit_menu.add_command(
            label="Select All",
            command=lambda: self.editor.tag_add("sel", "1.0", "end"),
        )
        edit_menu.add_separator()
        edit_menu.add_command(
            label="Find...", command=self._find_dialog, accelerator="Ctrl+F"
        )
        edit_menu.add_command(
            label="Replace...",
            command=self._replace_dialog,
            accelerator="Ctrl+H",
        )
        edit_menu.add_command(
            label="Go To Line...",
            command=self._goto_line,
            accelerator="Ctrl+L",
        )
        edit_menu.add_command(
            label="Toggle Comment",
            command=self._toggle_comment,
            accelerator="Ctrl+/",
        )
        edit_menu.add_command(label="Indent", command=self._indent_selection)
        edit_menu.add_command(label="Outdent", command=self._outdent_selection)
        edit_menu.add_command(
            label="Duplicate Line",
            command=self._duplicate_line,
            accelerator="Ctrl+D",
        )
        edit_menu.add_checkbutton(
            label="Word Wrap",
            command=self._toggle_wrap,
            variable=self.wrap_var,
        )
        edit_menu.add_separator()
        edit_menu.add_command(
            label="Preferences...",
            command=self._open_preferences,
        )

        # Run menu
        run_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Run", menu=run_menu)
        run_menu.add_command(label="Run", command=self._run, accelerator="F5")
        run_menu.add_command(
            label="Stop",
            command=self._stop,
            accelerator="Esc",
        )
        run_menu.add_separator()
        run_menu.add_command(
            label="Clear Console",
            command=self._clear_console,
        )
        run_menu.add_checkbutton(
            label="Trace Execution",
            variable=self.trace_var,
            command=self._on_trace_toggle,
        )

        # Debug menu
        debug_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Debug", menu=debug_menu)
        debug_menu.add_checkbutton(
            label="Trace Execution",
            variable=self.trace_var,
            command=self._on_trace_toggle,
        )
        debug_menu.add_separator()
        debug_menu.add_command(
            label="Insert TRACE",
            command=self._insert_trace,
            accelerator="Ctrl+Shift+T",
        )
        debug_menu.add_command(
            label="Insert DUMPVARS",
            command=self._insert_dumpvars,
            accelerator="Ctrl+Shift+V",
        )
        debug_menu.add_command(
            label="Insert PAUSE",
            command=self._insert_pause,
            accelerator="Ctrl+Shift+P",
        )

        # View menu
        view_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="View", menu=view_menu)
        view_menu.add_checkbutton(
            label="Show Line Numbers",
            variable=self.show_line_numbers_var,
            command=self._toggle_line_numbers,
        )
        theme_sub = tk.Menu(view_menu, tearoff=0)
        view_menu.add_cascade(label="Theme", menu=theme_sub)
        theme_sub.add_radiobutton(
            label="Light",
            value="light",
            variable=self.theme_var,
            command=lambda: self._set_theme("light"),
        )
        theme_sub.add_radiobutton(
            label="Dark",
            value="dark",
            variable=self.theme_var,
            command=lambda: self._set_theme("dark"),
        )
        font_sub = tk.Menu(view_menu, tearoff=0)
        view_menu.add_cascade(label="Editor Font", menu=font_sub)
        font_sub.add_command(
            label="Increase", command=lambda: self._bump_editor_font(+1)
        )
        font_sub.add_command(
            label="Decrease", command=lambda: self._bump_editor_font(-1)
        )
        cfont_sub = tk.Menu(view_menu, tearoff=0)
        view_menu.add_cascade(label="Console Font", menu=cfont_sub)
        cfont_sub.add_command(
            label="Increase", command=lambda: self._bump_console_font(+1)
        )
        cfont_sub.add_command(
            label="Decrease", command=lambda: self._bump_console_font(-1)
        )

        # Tools menu
        tools_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Tools", menu=tools_menu)
        tools_menu.add_command(
            label="Insert MySQL Boilerplate",
            command=self._insert_mysql_boilerplate,
        )
        tools_menu.add_command(
            label="Test MySQL Connection",
            command=self._test_mysql_connection,
        )

        # Examples menu
        examples_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Examples", menu=examples_menu)
        examples_menu.add_command(
            label="Hello World", command=self._load_default_example
        )
        examples_menu.add_command(
            label="Turtle Square", command=self._load_square_example
        )
        examples_menu.add_command(
            label="Procedures",
            command=self._load_proc_example,
        )
        examples_menu.add_separator()
        examples_menu.add_command(
            label="Rainbow Spiral",
            command=lambda: self._load_example_from_file("rainbow_spiral.tc"),
        )
        examples_menu.add_command(
            label="Fireworks",
            command=lambda: self._load_example_from_file("fireworks.tc"),
        )
        examples_menu.add_command(
            label="Bouncing Square",
            command=lambda: self._load_example_from_file("bouncing_square.tc"),
        )
        examples_menu.add_command(
            label="Color Waves",
            command=lambda: self._load_example_from_file("color_waves.tc"),
        )
        examples_menu.add_separator()
        examples_menu.add_command(
            label="MySQL Demo",
            command=lambda: self._load_example_from_file("mysql_demo.tc"),
        )

        # Help menu
        help_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Help", menu=help_menu)
        help_menu.add_command(label="About", command=self._show_about)

        # Toolbar
        toolbar = ttk.Frame(self)
        toolbar.pack(side="top", fill="x")

        ttk.Button(
            toolbar,
            text="New",
            command=self._new_file,
        ).pack(side="left")
        ttk.Button(
            toolbar,
            text="Open",
            command=self._open_file,
        ).pack(side="left")
        ttk.Button(
            toolbar,
            text="Save",
            command=self._save_file,
        ).pack(side="left")
        ttk.Button(
            toolbar,
            text="Save As",
            command=self._save_file_as,
        ).pack(side="left")
        ttk.Separator(toolbar, orient="vertical").pack(
            side="left",
            fill="y",
            padx=6,
        )
        ttk.Button(toolbar, text="Run", command=self._run).pack(side="left")
        ttk.Button(toolbar, text="Stop", command=self._stop).pack(side="left")
        ttk.Button(
            toolbar,
            text="Clear Console",
            command=self._clear_console,
        ).pack(side="left")
        # Removed redundant demo loader buttons from toolbar

        # Main panes
        panes = ttk.PanedWindow(self, orient="horizontal")
        panes.pack(fill="both", expand=True)

        # Editor on the left
        editor_frame = ttk.LabelFrame(panes, text="Code Editor")
        # Add line numbers
        self.line_num_frame = ttk.Frame(editor_frame)
        self.line_num_frame.pack(side="left", fill="y")
        self.line_numbers = tk.Text(
            self.line_num_frame,
            width=4,
            padx=3,
            takefocus=0,
            border=0,
            background="#f0f0f0",
            state="disabled",
            wrap="none",
            font="TkFixedFont",
        )
        self.line_numbers.pack(side="left", fill="y")

        self.editor = tk.Text(
            editor_frame,
            wrap="none",
            undo=True,
            font="TkFixedFont",
        )
        self.editor.pack(side="left", fill="both", expand=True)

        # Add scrollbars for editor
        editor_vscroll = ttk.Scrollbar(
            editor_frame, orient="vertical", command=self._on_editor_scroll
        )
        editor_vscroll.pack(side="right", fill="y")
        self.editor.configure(yscrollcommand=editor_vscroll.set)

        self._setup_highlighting()
        self.editor.bind("<KeyRelease>", self._on_editor_change)
        self.editor.bind("<Button-1>", self._on_editor_change)
        self._update_line_numbers()

        panes.add(editor_frame, weight=3)

        # Right side: canvas (top) + console (bottom)
        right = ttk.PanedWindow(panes, orient="vertical")
        canvas_frame = ttk.LabelFrame(right, text="Canvas")
        self.canvas = tk.Canvas(
            canvas_frame,
            width=500,
            height=400,
            bg="white",
        )
        self.canvas.pack(fill="both", expand=True)
        right.add(canvas_frame, weight=2)

        console_frame = ttk.LabelFrame(right, text="Console")
        # Use default themed colors and a fixed-width font for readability
        console_container = ttk.Frame(console_frame)
        console_container.pack(fill="both", expand=True)
        self.console = tk.Text(
            console_container,
            height=10,
            state="disabled",
            font="TkFixedFont",
        )
        vscroll = ttk.Scrollbar(
            console_container,
            orient="vertical",
            command=self.console.yview,
        )
        self.console.configure(yscrollcommand=vscroll.set)
        # Grid to allow scrollbar and text side-by-side
        console_container.columnconfigure(0, weight=1)
        console_container.rowconfigure(0, weight=1)
        self.console.grid(row=0, column=0, sticky="nsew")
        vscroll.grid(row=0, column=1, sticky="ns")
        right.add(console_frame, weight=1)

        panes.add(right, weight=2)

        # Status bar at the bottom
        self.status_bar = ttk.Frame(self)
        self.status_bar.pack(side="bottom", fill="x")

        self.status_label = ttk.Label(
            self.status_bar, text="Ready", relief="sunken", anchor="w"
        )
        self.status_label.pack(side="left", fill="x", expand=True)

        # State badge (theme/trace/line numbers)
        self.state_label = ttk.Label(
            self.status_bar,
            text="",
            relief="sunken",
            anchor="e",
            width=34,
        )
        self.state_label.pack(side="right")

        self.position_label = ttk.Label(
            self.status_bar, text="Line 1, Col 1", relief="sunken", width=15
        )
        self.position_label.pack(side="right")

    # ------------------------ Syntax highlighting ------------------------ #

    def _setup_highlighting(self) -> None:
        # Define tags
        self.editor.tag_configure("kw", foreground="#005cc5")
        self.editor.tag_configure("str", foreground="#22863a")
        self.editor.tag_configure("com", foreground="#6a737d")
        self.editor.tag_configure("lab", foreground="#b31d28")

        # Initial pass (key events are handled by _on_editor_change)
        self._highlight()

    def _update_line_numbers(self) -> None:
        """Update line numbers in the gutter."""
        if not self.show_line_numbers_var.get():
            return
        line_count = int(self.editor.index("end-1c").split(".", maxsplit=1)[0])
        line_num_text = "\n".join(str(i) for i in range(1, line_count + 1))

        self.line_numbers.configure(state="normal")
        self.line_numbers.delete("1.0", "end")
        self.line_numbers.insert("1.0", line_num_text)
        self.line_numbers.configure(state="disabled")

    def _on_editor_scroll(self, *args) -> None:
        """Sync editor and line numbers scrolling."""
        self.editor.yview(*args)
        if self.show_line_numbers_var.get():
            self.line_numbers.yview(*args)

    def _on_editor_change(self, event: Optional[tk.Event] = None) -> None:
        """Handle editor changes to update UI."""
        _ = event  # Unused but required by event binding
        self._update_line_numbers()
        self._update_status()
        self._highlight()

    # --- View helpers ---

    def _toggle_line_numbers(self) -> None:
        if self.show_line_numbers_var.get():
            # show
            # If frame got detached, re-pack
            self.line_num_frame.pack(side="left", fill="y")
            self.line_numbers.pack(side="left", fill="y")
            self._update_line_numbers()
        else:
            # hide
            try:
                self.line_num_frame.pack_forget()
            except tk.TclError:
                pass
        # Persist preference and update state badge
        self.settings["show_line_numbers"] = bool(self.show_line_numbers_var.get())
        self._save_settings()
        self._update_state_badge()

    def _set_theme(self, name: str) -> None:
        name = (name or "light").lower()
        self.settings["theme"] = name
        self.theme_var.set(name)
        self._apply_settings()
        self._update_state_badge()

    def _on_trace_toggle(self) -> None:
        """Update UI state when Trace Execution is toggled."""
        # Sync interpreter trace flag with current UI when available
        if self.interpreter is not None:
            try:
                self.interpreter.trace = bool(self.trace_var.get())
            except AttributeError:
                pass
        self._update_state_badge()

    def _update_state_badge(self) -> None:
        """Refresh the state badge showing Theme/Trace/Line Numbers."""
        theme = str(self.settings.get("theme", "light")).title()
        trace_on = "On" if bool(self.trace_var.get()) else "Off"
        ln_on = "On" if bool(self.show_line_numbers_var.get()) else "Off"
        text = f"Theme: {theme} | Trace: {trace_on} | Ln#: {ln_on}"
        try:
            self.state_label.configure(text=text)
        except tk.TclError:
            # If UI not ready, ignore
            pass

    def _edit_undo(self) -> None:
        try:
            self.editor.edit_undo()
        except tk.TclError:
            pass

    def _edit_redo(self) -> None:
        try:
            self.editor.edit_redo()
        except tk.TclError:
            pass

    def _bump_editor_font(self, delta: int) -> None:
        try:
            size = int(str(self.settings.get("editor_font_size", "11")))
        except (ValueError, TypeError):
            size = 11
        size = max(8, min(32, size + int(delta)))
        self.settings["editor_font_size"] = size
        self._apply_settings()

    def _bump_console_font(self, delta: int) -> None:
        try:
            size = int(str(self.settings.get("console_font_size", "11")))
        except (ValueError, TypeError):
            size = 11
        size = max(8, min(32, size + int(delta)))
        self.settings["console_font_size"] = size
        self._apply_settings()

    # --- Debug helpers ---

    def _insert_at_cursor(self, text: str) -> None:
        self.editor.insert("insert", text)

    def _insert_trace(self) -> None:
        self._insert_at_cursor("TRACE\n")

    def _insert_dumpvars(self) -> None:
        self._insert_at_cursor("DUMPVARS\n")

    def _insert_pause(self) -> None:
        self._insert_at_cursor("PAUSE\n")

    def _update_status(self) -> None:
        """Update cursor position in status bar."""
        cursor_pos = self.editor.index("insert")
        line, col = cursor_pos.split(".", maxsplit=1)
        self.position_label.configure(text=f"Line {line}, Col {int(col) + 1}")

    def _update_title(self) -> None:
        """Update window title with filename."""
        if self.current_file:
            filename = os.path.basename(self.current_file)
            self.root.title(f"Time Warp v3.0.0 - {filename}")
        else:
            self.root.title("Time Warp v3.0.0 - Untitled")

    def _highlight(self) -> None:
        text = self.editor.get("1.0", "end-1c")
        # Clear tags
        for tag in ("kw", "str", "com", "lab"):
            self.editor.tag_remove(tag, "1.0", "end")
        # Use precompiled module-level patterns
        _spat = _STRING_PATTERN
        for m in _spat.finditer(text):
            start = f"1.0+{m.start()}c"
            end = f"1.0+{m.end()}c"
            self.editor.tag_add("str", start, end)

        # Comments (REM ... or # ... at line start ignoring spaces)
        line_start = 0
        for line in text.splitlines(True):
            stripped = line.lstrip()
            if stripped.upper().startswith("REM") or stripped.startswith("#"):
                a = line_start + (len(line) - len(stripped))
                b = line_start + len(line)
                self.editor.tag_add("com", f"1.0+{a}c", f"1.0+{b}c")
            # Labels: ^name:
            m_label = re.match(r"\s*([A-Za-z_]\w*):", line)
            if m_label:
                a = line_start + m_label.start(1)
                b = line_start + m_label.end(1)
                self.editor.tag_add("lab", f"1.0+{a}c", f"1.0+{b}c")
            line_start += len(line)

        # Keywords (case-insensitive) using module-level pattern
        for m in _KW_PATTERN.finditer(text):
            start = f"1.0+{m.start()}c"
            end = f"1.0+{m.end()}c"
            self.editor.tag_add("kw", start, end)

    # ----------------------------- Actions ----------------------------- #

    def _new_file(self) -> None:
        self.current_file = None
        self.editor.delete("1.0", "end")
        self.console.configure(state="normal")
        self.console.delete("1.0", "end")
        self.console.configure(state="disabled")
        self.canvas.delete("all")
        self._update_title()
        self.status_label.configure(text="New file created")

    def _open_file(self) -> None:
        path = filedialog.askopenfilename(
            title="Open TempleCode file",
            filetypes=[("TempleCode", "*.tc"), ("All", "*.*")],
        )
        if not path:
            return
        try:
            with open(path, "r", encoding="utf-8") as f:
                self.editor.delete("1.0", "end")
                self.editor.insert("1.0", f.read())
                self.current_file = path
                self._update_title()
                self.status_label.configure(text=f"Opened: {path}")
        except OSError as e:
            messagebox.showerror("Open failed", str(e))

    def _save_file(self) -> None:
        if self.current_file:
            try:
                with open(self.current_file, "w", encoding="utf-8") as f:
                    f.write(self.editor.get("1.0", "end-1c"))
                self.status_label.configure(text=f"Saved: {self.current_file}")
            except OSError as e:
                messagebox.showerror("Save failed", str(e))
        else:
            self._save_file_as()

    def _save_file_as(self) -> None:
        path = filedialog.asksaveasfilename(
            title="Save TempleCode file",
            defaultextension=".tc",
            filetypes=[("TempleCode", "*.tc"), ("All", "*.*")],
        )
        if not path:
            return
        try:
            with open(path, "w", encoding="utf-8") as f:
                f.write(self.editor.get("1.0", "end-1c"))
            self.current_file = path
            self._update_title()
            self.status_label.configure(text=f"Saved: {path}")
        except OSError as e:
            messagebox.showerror("Save failed", str(e))

    def _clear_console(self) -> None:
        """Clear the console output."""
        self.console.configure(state="normal")
        self.console.delete("1.0", "end")
        self.console.configure(state="disabled")
        self.status_label.configure(text="Console cleared")

    def _run(self) -> None:
        if self.runner_thread and self.runner_thread.is_alive():
            messagebox.showinfo("Running", "Program already running.")
            return

        self.stop_event.clear()
        self.console.configure(state="normal")
        self.console.delete("1.0", "end")
        self.console.configure(state="disabled")
        self.canvas.delete("all")
        self.status_label.configure(text="Running...")

        io = TextIO(self.console, self.root)
        turtle = CanvasTurtle(self.canvas)
        self.interpreter = TempleInterpreter(
            io=io, turtle=turtle, stop_flag=self.stop_event
        )
        # Apply trace setting
        interp_set = self.interpreter
        if interp_set is not None:
            interp_set.trace = bool(self.trace_var.get())

        code = self.editor.get("1.0", "end-1c")

        def runner():
            interp = self.interpreter
            if interp is not None:
                try:
                    interp.run(code)

                    def _mark_done() -> None:
                        self.status_label.configure(text="Program completed")

                    self.root.after(0, _mark_done)
                except (
                    ValueError,
                    TypeError,
                    ImportError,
                    ModuleNotFoundError,
                    RuntimeError,
                    ConnectionError,
                ) as err:
                    err_msg = str(err)

                    def _mark_err() -> None:
                        self.status_label.configure(text=f"Error: {err_msg}")

                    self.root.after(0, _mark_err)

        self.runner_thread = threading.Thread(target=runner, daemon=True)
        self.runner_thread.start()

    def _stop(self) -> None:
        if self.stop_event:
            self.stop_event.set()
            self.status_label.configure(text="Program stopped")

    # ----------------------- Example loaders ----------------------- #

    def _load_example_from_file(self, filename: str) -> None:
        """Load an example from the repo examples folder into the editor.

        The examples folder is at project_root/examples.
        """
        # Determine examples directory relative to this file
        # time_warp/ -> project root -> examples
        project_root = os.path.abspath(
            os.path.join(os.path.dirname(__file__), os.pardir, os.pardir)
        )
        examples_dir = os.path.join(project_root, "examples")
        path = os.path.join(examples_dir, filename)
        if not os.path.isfile(path):
            messagebox.showerror(
                "Example not found",
                f"Couldn't find example: {path}",
            )
            return
        try:
            with open(path, "r", encoding="utf-8") as f:
                self.editor.delete("1.0", "end")
                self.editor.insert("1.0", f.read())
                # Don't set current_file so edits won't overwrite example
                self.current_file = None
                self._update_title()
                self.status_label.configure(text=f"Loaded example: {filename}")
        except OSError as e:
            messagebox.showerror("Failed to load example", str(e))

    def _load_default_example(self) -> None:
        sample = (
            'PRINT "Welcome to Time Warp v3.0.0!"\n'
            'PRINT "Type Run to execute, or try the Square example."\n'
            "\n"
            "REM Input and basic math\n"
            'INPUT name "Your name? "\n'
            'PRINT "Hello, " + name\n'
            "LET x = 3 * (2 + 4)\n"
            "PRINT x\n"
        )
        self.editor.delete("1.0", "end")
        self.editor.insert("1.0", sample)
        self.current_file = None
        self._update_title()
        self.status_label.configure(text="Loaded Hello example")

    def _load_square_example(self) -> None:
        sample = "CLS\nREPEAT 4\n  FD 120\n  LT 90\nENDREPEAT\n"
        self.editor.delete("1.0", "end")
        self.editor.insert("1.0", sample)
        self.current_file = None
        self._update_title()
        self.status_label.configure(text="Loaded Square example")

    def _load_proc_example(self) -> None:
        sample = (
            "REM Procedure demonstration\n"
            "\n"
            "PROC greet name\n"
            '  PRINT "Hello, " + name + "!"\n'
            "ENDPROC\n"
            "\n"
            "PROC square n\n"
            "  RETURN n * n\n"
            "ENDPROC\n"
            "\n"
            "PROC factorial n\n"
            "  IF n <= 1 THEN RETURN 1\n"
            "  LET prev = 0\n"
            "  CALL factorial n - 1 INTO prev\n"
            "  RETURN n * prev\n"
            "ENDPROC\n"
            "\n"
            'CALL greet "World"\n'
            "CALL square 7 INTO result\n"
            'PRINT "7 squared = " + str(result)\n'
            "CALL factorial 5 INTO fact\n"
            'PRINT "5! = " + str(fact)\n'
        )
        self.editor.delete("1.0", "end")
        self.editor.insert("1.0", sample)
        self.current_file = None
        self._update_title()
        self.status_label.configure(text="Loaded Procedure example")

    def _insert_mysql_boilerplate(self) -> None:
        """Insert a MySQL demo snippet using saved preferences."""
        host = str(self.settings.get("db_host", "localhost"))
        user = str(self.settings.get("db_user", "root"))
        pwd = str(self.settings.get("db_password", ""))
        dbn = str(self.settings.get("db_name", "test"))
        _pval = self.settings.get("db_port", 3306)
        try:
            port = int(str(_pval))
        except (ValueError, TypeError):
            port = 3306
        snippet = (
            "REM MySQL boilerplate inserted by IDE\n"
            "CLS\n\n"
            f'LET host = "{host}"\n'
            f'LET user = "{user}"\n'
            f'LET password = "{pwd}"\n'
            f'LET dbname = "{dbn}"\n'
            f"LET port = {port}\n\n"
            "DBCONNECT main, host, user, password, dbname, port\n"
            "REM ... your DBEXEC/DBQUERY code here ...\n"
            "DBDISCONNECT main\n"
        )
        self.editor.insert("insert", snippet)
        self.status_label.configure(text="Inserted MySQL boilerplate")

    def _test_mysql_connection(self) -> None:
        """Test MySQL connection with saved preferences."""
        if self.runner_thread and self.runner_thread.is_alive():
            messagebox.showinfo(
                "Running",
                "Please stop the current program "
                + "before testing the DB connection.",
            )
            return

        host = str(self.settings.get("db_host", "localhost"))
        user = str(self.settings.get("db_user", "root"))
        pwd = str(self.settings.get("db_password", ""))
        dbn = str(self.settings.get("db_name", "test"))
        _pval = self.settings.get("db_port", 3306)
        try:
            port = int(str(_pval))
        except (ValueError, TypeError):
            port = 3306

        # Prepare console
        self.console.configure(state="normal")
        self.console.delete("1.0", "end")
        self.console.configure(state="disabled")
        self.status_label.configure(text="Testing MySQL connection...")

        io = TextIO(self.console, self.root)
        turtle = CanvasTurtle(self.canvas)
        self.interpreter = TempleInterpreter(
            io=io,
            turtle=turtle,
            stop_flag=self.stop_event,
        )
        interp_set = self.interpreter
        if interp_set is not None:
            interp_set.trace = bool(self.trace_var.get())

        # Minimal TempleCode snippet to validate connectivity
        code = (
            "REM IDE: Test MySQL connectivity\n"
            f'LET host = "{host}"\n'
            f'LET user = "{user}"\n'
            f'LET password = "{pwd}"\n'
            f'LET dbname = "{dbn}"\n'
            f"LET port = {port}\n"
            "DBCONNECT test, host, user, password, dbname, port\n"
            'DBQUERY test, "SELECT 1 AS ok" INTO rows\n'
            'PRINT "Connected. SELECT 1 => " + str(rows) + "\n"\n'
            "DBDISCONNECT test\n"
        )

        def runner():
            interp = self.interpreter
            if interp is not None:
                try:
                    interp.run(code)

                    def _mark_mysql_ok2() -> None:
                        self.status_label.configure(text="MySQL connection OK")

                    self.root.after(0, _mark_mysql_ok2)
                except (
                    ValueError,
                    TypeError,
                    ImportError,
                    ModuleNotFoundError,
                    RuntimeError,
                    ConnectionError,
                ) as err:
                    err_msg = str(err)
                    # Print error details to console and show concise status
                    io.write(f"MySQL connection failed: {err_msg}\n")

                    def _mark_mysql_fail2() -> None:
                        self.status_label.configure(text="MySQL connection failed")

                    self.root.after(0, _mark_mysql_fail2)

        self.runner_thread = threading.Thread(target=runner, daemon=True)
        self.runner_thread.start()

    def _show_about(self) -> None:
        """Show about dialog."""
        about_text = (
            "Time Warp IDE v3.0.0\n\n"
            "A modern IDE for TempleCode - a language blending\n"
            "BASIC, PILOT, and Logo.\n\n"
            "Features:\n"
            "• Procedures with parameters and return values\n"
            "• Turtle graphics\n"
            "• File and CSV I/O\n"
            "• Built-in utilities and math functions\n\n"
            "Created for intuitive and educational programming."
        )
        messagebox.showinfo("About Time Warp", about_text)

    # ----------------------- Editor utilities ----------------------- #

    def _get_selection_range(self) -> tuple[str, str]:
        """Return selection range or current line bounds."""
        try:
            start = self.editor.index("sel.first")
            end = self.editor.index("sel.last")
        except tk.TclError:
            line = self.editor.index("insert").split(".", maxsplit=1)[0]
            return f"{line}.0", f"{line}.end"
        s_line = start.split(".", maxsplit=1)[0]
        e_line = end.split(".", maxsplit=1)[0]
        return f"{s_line}.0", f"{e_line}.end"

    def _find_dialog(self) -> None:
        win = tk.Toplevel(self.root)
        win.title("Find")
        ttk.Label(win, text="Find:").grid(row=0, column=0, padx=6, pady=6)
        pattern = tk.StringVar()
        entry = ttk.Entry(win, textvariable=pattern, width=32)
        entry.grid(row=0, column=1, padx=6, pady=6)

        def do_find() -> None:
            self.editor.tag_remove("sel", "1.0", "end")
            pat = pattern.get()
            if not pat:
                return
            idx = self.editor.search(pat, "insert", stopindex="end")
            if idx:
                last = f"{idx}+{len(pat)}c"
                self.editor.tag_add("sel", idx, last)
                self.editor.mark_set("insert", last)
                self.editor.see(idx)

        ttk.Button(win, text="Find Next", command=do_find).grid(
            row=0, column=2, padx=6, pady=6
        )
        entry.focus_set()

    def _replace_dialog(self) -> None:
        win = tk.Toplevel(self.root)
        win.title("Replace")
        ttk.Label(win, text="Find:").grid(row=0, column=0, padx=6, pady=6)
        find_var = tk.StringVar()
        ttk.Entry(win, textvariable=find_var, width=32).grid(
            row=0, column=1, padx=6, pady=6
        )
        ttk.Label(win, text="Replace:").grid(row=1, column=0, padx=6, pady=6)
        repl_var = tk.StringVar()
        ttk.Entry(win, textvariable=repl_var, width=32).grid(
            row=1, column=1, padx=6, pady=6
        )

        def do_find_next() -> None:
            pat = find_var.get()
            if not pat:
                return
            idx = self.editor.search(pat, "insert", stopindex="end")
            if idx:
                last = f"{idx}+{len(pat)}c"
                self.editor.tag_remove("sel", "1.0", "end")
                self.editor.tag_add("sel", idx, last)
                self.editor.mark_set("insert", last)
                self.editor.see(idx)

        def do_replace() -> None:
            try:
                s = self.editor.index("sel.first")
                e = self.editor.index("sel.last")
            except tk.TclError:
                do_find_next()
                return
            self.editor.delete(s, e)
            self.editor.insert(s, repl_var.get())

        def do_replace_all() -> None:
            pat = find_var.get()
            repl = repl_var.get()
            if not pat:
                return
            start = "1.0"
            while True:
                idx = self.editor.search(pat, start, stopindex="end")
                if not idx:
                    break
                last = f"{idx}+{len(pat)}c"
                self.editor.delete(idx, last)
                self.editor.insert(idx, repl)
                start = f"{idx}+{len(repl)}c"

        btns = ttk.Frame(win)
        btns.grid(row=2, column=0, columnspan=3, pady=6)
        ttk.Button(btns, text="Find Next", command=do_find_next).pack(
            side="left", padx=4
        )
        ttk.Button(btns, text="Replace", command=do_replace).pack(
            side="left",
            padx=4,
        )
        ttk.Button(btns, text="Replace All", command=do_replace_all).pack(
            side="left", padx=4
        )

    def _goto_line(self) -> None:
        line_str = simpledialog.askstring(
            "Go To Line", "Line number:", parent=self.root
        )
        if not line_str:
            return
        try:
            line_num = int(line_str)
        except ValueError:
            return
        idx = f"{line_num}.0"
        self.editor.mark_set("insert", idx)
        self.editor.see(idx)
        self._update_status()

    def _toggle_comment(self) -> None:
        start, end = self._get_selection_range()
        start_line = int(start.split(".", maxsplit=1)[0])
        end_line = int(end.split(".", maxsplit=1)[0])
        for ln in range(start_line, end_line + 1):
            a = f"{ln}.0"
            b = f"{ln}.end"
            text = self.editor.get(a, b)
            stripped = text.lstrip()
            if not text:
                continue
            if stripped.startswith("#"):
                off = text.find("#")
                if off >= 0:
                    if off + 1 < len(text) and text[off + 1] == " ":
                        self.editor.delete(f"{ln}.{off}", f"{ln}.{off+2}")
                    else:
                        self.editor.delete(f"{ln}.{off}")
            elif stripped.upper().startswith("REM"):
                off = text.upper().find("REM")
                if off >= 0:
                    end_off = off + 3
                    if end_off < len(text) and text[end_off] == " ":
                        end_off += 1
                    self.editor.delete(f"{ln}.{off}", f"{ln}.{end_off}")
            else:
                self.editor.insert(a, "# ")

    def _indent_selection(self) -> None:
        start, end = self._get_selection_range()
        line_start = int(start.split(".", maxsplit=1)[0])
        line_end = int(end.split(".", maxsplit=1)[0])
        for ln in range(line_start, line_end + 1):
            self.editor.insert(f"{ln}.0", "    ")

    def _outdent_selection(self) -> None:
        start, end = self._get_selection_range()
        line_start = int(start.split(".", maxsplit=1)[0])
        line_end = int(end.split(".", maxsplit=1)[0])
        for ln in range(line_start, line_end + 1):
            a = f"{ln}.0"
            text = self.editor.get(a, f"{ln}.4")
            if text.startswith("\t"):
                self.editor.delete(a, f"{ln}.1")
            elif text.startswith("    "):
                self.editor.delete(a, f"{ln}.4")

    def _duplicate_line(self) -> None:
        line = self.editor.index("insert").split(".", maxsplit=1)[0]
        a = f"{line}.0"
        b = f"{line}.end"
        text = self.editor.get(a, b)
        self.editor.insert(f"{line}.end", "\n" + text)

    def _toggle_wrap(self) -> None:
        current = self.editor.cget("wrap")
        new_wrap = "word" if current == "none" else "none"
        self.editor.configure(wrap=new_wrap)  # type: ignore
        self.wrap_var.set(new_wrap != "none")

    # ----------------------- Preferences & Settings ----------------------- #

    def _config_path(self) -> str:
        base = os.path.expanduser("~/.time_warp")
        os.makedirs(base, exist_ok=True)
        return os.path.join(base, "config.json")

    def _load_settings(self) -> None:

        path = self._config_path()
        try:
            if os.path.isfile(path):
                with open(path, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    if isinstance(data, dict):
                        self.settings.update(data)
        except (OSError, json.JSONDecodeError, ValueError):
            # Ignore settings load errors
            pass

    def _save_settings(self) -> None:

        data = dict(self.settings)
        data["geometry"] = self.root.geometry()
        try:
            with open(self._config_path(), "w", encoding="utf-8") as f:
                json.dump(data, f, indent=2)
        except (OSError, TypeError):
            pass

    def _apply_settings(self) -> None:
        # Apply geometry if saved
        geom = self.settings.get("geometry")
        if geom:
            try:
                self.root.geometry(str(geom))
            except tk.TclError:
                pass

        # Fonts
        try:
            ed_size = int(str(self.settings.get("editor_font_size", "11")))
            co_size = int(str(self.settings.get("console_font_size", "11")))
        except (ValueError, TypeError):
            ed_size, co_size = 11, 11
        self.editor.configure(font=("TkFixedFont", ed_size))
        self.console.configure(font=("TkFixedFont", co_size))
        self.line_numbers.configure(font=("TkFixedFont", ed_size))

        # Theme
        theme = str(self.settings.get("theme", "light")).lower()
        # keep View menu theme radio in sync
        try:
            self.theme_var.set(theme)
        except tk.TclError:
            pass
        if theme == "dark":
            ed_bg, ed_fg = "#1e1e1e", "#d4d4d4"
            ln_bg, ln_fg = "#2b2b2b", "#bbbbbb"
            co_bg, co_fg = "#1e1e1e", "#d4d4d4"
        else:
            ed_bg, ed_fg = "white", "black"
            ln_bg, ln_fg = "#f0f0f0", "black"
            co_bg, co_fg = "white", "black"
        self.editor.configure(bg=ed_bg, fg=ed_fg, insertbackground=ed_fg)
        self.console.configure(bg=co_bg, fg=co_fg, insertbackground=co_fg)
        self.line_numbers.configure(bg=ln_bg, fg=ln_fg)

        # Line numbers visibility from settings
        try:
            show_ln = bool(self.settings.get("show_line_numbers", True))
            self.show_line_numbers_var.set(show_ln)
            self._toggle_line_numbers()
        except tk.TclError:
            pass

    def _open_preferences(self) -> None:
        win = tk.Toplevel(self.root)
        win.title("Preferences")
        win.resizable(False, False)

        ttk.Label(win, text="Theme:").grid(
            row=0,
            column=0,
            padx=8,
            pady=6,
            sticky="e",
        )
        theme_val = str(self.settings.get("theme", "light"))
        theme_var = tk.StringVar(value=theme_val)
        ttk.OptionMenu(win, theme_var, theme_var.get(), "light", "dark").grid(
            row=0, column=1, padx=8, pady=6, sticky="w"
        )

        ttk.Label(win, text="Editor font size:").grid(
            row=1, column=0, padx=8, pady=6, sticky="e"
        )
        ed_size = int(str(self.settings.get("editor_font_size", "11")))
        ed_var = tk.IntVar(value=ed_size)
        ttk.Spinbox(win, from_=8, to=32, textvariable=ed_var, width=6).grid(
            row=1, column=1, padx=8, pady=6, sticky="w"
        )

        ttk.Label(win, text="Console font size:").grid(
            row=2, column=0, padx=8, pady=6, sticky="e"
        )
        co_size = int(str(self.settings.get("console_font_size", "11")))
        co_var = tk.IntVar(value=co_size)
        ttk.Spinbox(win, from_=8, to=32, textvariable=co_var, width=6).grid(
            row=2, column=1, padx=8, pady=6, sticky="w"
        )

        # Default MySQL connection
        ttk.Label(win, text="DB host:").grid(
            row=3, column=0, padx=8, pady=6, sticky="e"
        )
        db_host = str(self.settings.get("db_host", "localhost"))
        db_host_var = tk.StringVar(value=db_host)
        ttk.Entry(win, textvariable=db_host_var, width=24).grid(
            row=3, column=1, padx=8, pady=6, sticky="w"
        )

        ttk.Label(win, text="DB user:").grid(
            row=4, column=0, padx=8, pady=6, sticky="e"
        )
        db_user = str(self.settings.get("db_user", "root"))
        db_user_var = tk.StringVar(value=db_user)
        ttk.Entry(win, textvariable=db_user_var, width=24).grid(
            row=4, column=1, padx=8, pady=6, sticky="w"
        )

        ttk.Label(win, text="DB password:").grid(
            row=5, column=0, padx=8, pady=6, sticky="e"
        )
        db_pwd = str(self.settings.get("db_password", ""))
        db_pwd_var = tk.StringVar(value=db_pwd)
        ttk.Entry(win, textvariable=db_pwd_var, width=24, show="*").grid(
            row=5, column=1, padx=8, pady=6, sticky="w"
        )

        ttk.Label(win, text="DB name:").grid(
            row=6, column=0, padx=8, pady=6, sticky="e"
        )
        db_name = str(self.settings.get("db_name", "test"))
        db_name_var = tk.StringVar(value=db_name)
        ttk.Entry(win, textvariable=db_name_var, width=24).grid(
            row=6, column=1, padx=8, pady=6, sticky="w"
        )

        ttk.Label(win, text="DB port:").grid(
            row=7, column=0, padx=8, pady=6, sticky="e"
        )
        _pref_port = self.settings.get("db_port", 3306) or 3306
        db_port_var = tk.IntVar(value=int(str(_pref_port)))
        ttk.Spinbox(
            win,
            from_=1,
            to=65535,
            textvariable=db_port_var,
            width=8,
        ).grid(row=7, column=1, padx=8, pady=6, sticky="w")

        btns = ttk.Frame(win)
        btns.grid(row=8, column=0, columnspan=2, pady=10)

        def _test_prefs_connection() -> None:
            """Test DB connection using current dialog values (no save)."""
            # Don't interfere with a running program
            if self.runner_thread and self.runner_thread.is_alive():
                messagebox.showinfo(
                    "Running",
                    "Please stop the current program "
                    + "before testing the DB connection.",
                )
                return

            host = db_host_var.get()
            user = db_user_var.get()
            pwd = db_pwd_var.get()
            dbn = db_name_var.get()
            try:
                port_val = int(str(db_port_var.get()))
            except (ValueError, TypeError):
                port_val = 3306

            # Prepare console
            self.console.configure(state="normal")
            self.console.delete("1.0", "end")
            self.console.configure(state="disabled")
            self.status_label.configure(text="Testing MySQL connection...")

            io = TextIO(self.console, self.root)
            turtle = CanvasTurtle(self.canvas)
            self.interpreter = TempleInterpreter(
                io=io,
                turtle=turtle,
                stop_flag=self.stop_event,
            )
            self.interpreter.trace = bool(self.trace_var.get())

            code = (
                "REM IDE: Test MySQL connectivity (Preferences)\n"
                f'LET host = "{host}"\n'
                f'LET user = "{user}"\n'
                f'LET password = "{pwd}"\n'
                f'LET dbname = "{dbn}"\n'
                f"LET port = {port_val}\n"
                "DBCONNECT test, host, user, password, dbname, port\n"
                'DBQUERY test, "SELECT 1 AS ok" INTO rows\n'
                'PRINT "Connected. SELECT 1 => " + str(rows) + "\n"\n'
                "DBDISCONNECT test\n"
            )

            def runner():
                interp = self.interpreter
                if interp is not None:
                    try:
                        interp.run(code)

                        def _mark_mysql_ok2() -> None:
                            self.status_label.configure(text="MySQL connection OK")

                        self.root.after(0, _mark_mysql_ok2)
                    except (
                        ValueError,
                        TypeError,
                        ImportError,
                        ModuleNotFoundError,
                        RuntimeError,
                        ConnectionError,
                    ) as err:
                        err_msg = str(err)
                        io.write(f"MySQL connection failed: {err_msg}\n")

                        def _mark_mysql_fail2() -> None:
                            self.status_label.configure(text="MySQL connection failed")

                        self.root.after(0, _mark_mysql_fail2)

            self.runner_thread = threading.Thread(target=runner, daemon=True)
            self.runner_thread.start()

        ttk.Button(btns, text="Cancel", command=win.destroy).pack(
            side="right",
            padx=6,
        )

        def save_and_close() -> None:
            self.settings["theme"] = theme_var.get()
            self.settings["editor_font_size"] = int(ed_var.get())
            self.settings["console_font_size"] = int(co_var.get())
            # Save DB defaults
            self.settings["db_host"] = db_host_var.get()
            self.settings["db_user"] = db_user_var.get()
            self.settings["db_password"] = db_pwd_var.get()
            self.settings["db_name"] = db_name_var.get()
            try:
                self.settings["db_port"] = int(db_port_var.get())
            except (ValueError, TypeError):
                self.settings["db_port"] = 3306
            self._apply_settings()
            self._save_settings()
            win.destroy()

        ttk.Button(btns, text="OK", command=save_and_close).pack(side="right")
        ttk.Button(
            btns,
            text="Test Connection",
            command=_test_prefs_connection,
        ).pack(side="left", padx=6)

    def _on_close(self) -> None:
        self._save_settings()
        self.root.destroy()


def main() -> None:
    root = tk.Tk()
    app = TimeWarpApp(root)
    app.mainloop()


if __name__ == "__main__":
    main()
