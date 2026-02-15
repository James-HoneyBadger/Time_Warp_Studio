# FAQ - Frequently Asked Questions

Common questions and answers about Time Warp Studio.

---

## General Questions

### Q: What is Time Warp Studio?

**A:** Time Warp Studio is an educational programming IDE that supports 7 languages: BASIC, Logo, PILOT, C, Pascal, Prolog, and Forth. It combines classic educational programming languages with modern IDE features including graphics, debugging, and lessons.

### Q: Can I use it for free?

**A:** Yes! Time Warp Studio is open-source and free to use under the MIT License.

### Q: What are the system requirements?

**A:** 
- Python 3.10+
- 4GB RAM minimum (8GB recommended)
- Modern OS (Windows 10+, macOS 10.14+, Linux Ubuntu 20.04+)
- CPU with SSSE3/SSE4 support (most modern CPUs)

### Q: Is it cross-platform?

**A:** Yes. Time Warp Studio runs on Windows, macOS, and Linux with the same interface.

---

## Installation & Setup

### Q: How do I install Time Warp Studio?

**A:**
```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
pip install -r requirements.txt
python Platforms/Python/time_warp_ide.py
```

### Q: I'm getting "Illegal instruction" error. What should I do?

**A:** Your CPU doesn't support required features (SSSE3/SSE4). Solutions:
- Use modern computer or laptop
- Run on cloud VM with compatible CPU
- Use WSL 2 on Windows
- Contact maintainer for compatibility

### Q: Can I run it without installing Python?

**A:** You need Python 3.10+. Install from python.org or use your OS package manager.

### Q: Where does it store my files and settings?

**A:** Settings stored in `~/.Time_Warp/config.json`. Files saved wherever you choose.

---

## Language Questions

### Q: Which language should I learn first?

**A:** 
- **Beginners**: Start with Logo (visual/graphics)
- **Classic**: Try BASIC for traditional programming
- **Modern**: Use C or Pascal for structured programming
- **Advanced**: Learn Prolog for logic programming

### Q: Can I mix languages in one file?

**A:** No, each file is one language. Create separate files for different languages.

### Q: Why is my BASIC program saying "DIM syntax error"?

**A:** BASIC array syntax is `DIM ARRAY(10)` not `DIM ARRAY[10]`. Use parentheses.

### Q: Can I use libraries and imports?

**A:**
- **BASIC**: Limited libraries (math, string functions)
- **C**: stdio.h, math.h available
- **Pascal**: Standard library functions
- **Logo/PILOT/Prolog/Forth**: No external libraries

### Q: How do I define functions in BASIC?

**A:** Use subroutines:
```basic
GOSUB MYLABEL
PRINT RESULT
END

MYLABEL:
    RESULT = X * 2
RETURN
```

---

## Graphics Questions

### Q: How do I use turtle graphics?

**A:** Use Logo language or graphics commands in BASIC:
```logo
FORWARD 100
RIGHT 90
CIRCLE 50
```

### Q: My graphics won't display. Why?

**A:** 
- Make sure to use Logo language (.logo file)
- Or use BASIC with SCREEN command
- Check console for errors
- Canvas might be zoomed out - use scroll wheel to zoom

### Q: Can I save my drawings as images?

**A:** Not yet. Current version displays on canvas. Save code instead.

### Q: What colors can I use?

**A:** 
- Named colors: `SETPENCOLOR 255` (0-255 scale)
- RGB: `SETPENCOLOR 255 0 0` (Red Green Blue)
- See TURTLE_GRAPHICS.md for details

---

## Debugging Questions

### Q: How do I debug my program?

**A:** Use the integrated debugger:
1. Set breakpoint (Ctrl+B)
2. Run program (Ctrl+R)
3. Step through (F10/F11)
4. Watch variables change
5. View timeline after execution

### Q: Why can't I set a breakpoint?

**A:** Breakpoints work on executable lines only. Click line number in left margin.

### Q: How do I see variable values?

**A:** 
- Open Variables Inspector panel
- Hover over variable in code
- Use debugger to step through

### Q: What's the difference between Step Into and Step Over?

**A:**
- **Step Into** (F11): Enter functions/procedures
- **Step Over** (F10): Skip function execution
- Use Step Into to debug functions, Step Over to skip them

---

## Performance Questions

### Q: Why is the IDE slow?

**A:**
- First startup: Check system specs
- Large programs: Reduce canvas drawing complexity
- Many variables: Close unused panels
- See TROUBLESHOOTING.md for optimization tips

### Q: What's the maximum program size?

**A:** Depends on RAM. Typically:
- Simple programs: unlimited
- Graphics-heavy: 10,000+ turtle commands
- Variables: millions possible (limited by RAM)
- Array size: limited by RAM

### Q: Can I run multiple programs simultaneously?

**A:** No, only one program at a time. Stop current, then run next.

---

## File & Project Questions

### Q: How do I organize multiple files?

**A:** Use Project Runner panel:
1. Create folder for project
2. Save multiple .bas, .logo files
3. Use Project Runner to manage
4. Run individual files or batch

### Q: Can I open multiple files at once?

**A:** Currently one file open at a time. Switch via File → Recent or File → Open.

### Q: What file formats are supported?

**A:**
- `.bas` - BASIC
- `.logo` - Logo
- `.pilot` - PILOT
- `.c` - C
- `.pas` - Pascal
- `.pro` - Prolog
- `.f` - Forth

### Q: How do I export my program?

**A:** 
```
File → Export
```

Save as text or PDF (PDF in development).

---

## UI & Customization Questions

### Q: How do I change the theme?

**A:**
```
View → Theme → [Select theme]
```

8 themes available: Dracula, Monokai, Solarized, Ocean, Spring, Sunset, Candy, Forest

### Q: Can I customize the editor font?

**A:**
```
Edit → Preferences → Editor
```

Change font family, size, and tab width.

### Q: How do I show/hide panels?

**A:**
```
View → [Panel name] [Toggle]
```

Or click panel tabs at bottom right.

### Q: Why is the canvas so small?

**A:** Zoom with scroll wheel on canvas area. Or View → Zoom In.

### Q: Can I make the window bigger?

**A:** Yes, resize window normally. Panels adapt to window size.

---

## Learning Questions

### Q: Where are the lessons?

**A:**
```
File → Lessons
```

Or open Lesson Mode panel.

### Q: Are there example programs?

**A:**
```
File → Examples [Ctrl+E]
```

86+ working examples in Examples/ directory.

### Q: How do I copy an example?

**A:**
1. File → Examples
2. Select example
3. Program loads in editor
4. Modify and save as new file

### Q: Can I create my own lessons?

**A:** Not yet in current version. Contact maintainer for future lesson creation tools.

---

## Error Messages

### Q: "SyntaxError: Invalid syntax"

**A:** Your code has language syntax error. Check LANGUAGE_GUIDE.md for correct syntax.

### Q: "Undefined variable"

**A:** Used variable before assigning value. In BASIC: use DIM or assign first.

### Q: "Stack overflow"

**A:** Infinite recursion or loop. Use debugger to find loop point.

### Q: "Out of memory"

**A:** Program uses too much RAM. Reduce array size or number of variables.

---

## Contributing & Bug Reports

### Q: How do I report a bug?

**A:**
1. Open GitHub Issues
2. Describe bug clearly
3. Include code example
4. List OS and Python version
5. Attach screenshot if relevant

### Q: Can I contribute code?

**A:** Yes! See CONTRIBUTING.md for guidelines:
1. Fork repository
2. Create feature branch
3. Make changes
4. Test thoroughly
5. Submit pull request

### Q: How do I request a feature?

**A:** 
1. Check existing GitHub Issues
2. Create new issue with "Feature Request" label
3. Describe use case clearly
4. Provide example if possible

---

## Miscellaneous

### Q: Is my code saved automatically?

**A:** You must Save manually (Ctrl+S). There's no auto-save yet.

### Q: Can I use this for teaching?

**A:** Yes! Perfect for classrooms. Features:
- Multi-file projects
- Example programs
- Lesson system
- Classroom mode (in development)

### Q: What's the license?

**A:** MIT License - free for personal, educational, commercial use.

### Q: Who maintains this project?

**A:** James Temple <james@honey-badger.org>

### Q: Where can I get help?

**A:**
- GitHub Issues - Report bugs
- GitHub Discussions - Ask questions
- Documentation - See docs/ folder
- Email - james@honey-badger.org

---

**For more help:**
- [README.md](../README.md) - Project overview
- [USER_GUIDE.md](USER_GUIDE.md) - IDE usage
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - Language syntax
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Problem solving
