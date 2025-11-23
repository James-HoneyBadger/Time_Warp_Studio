# Time Warp IDE Documentation

**Professional, comprehensive documentation for Time Warp IDE**

This directory contains reorganized and enhanced documentation suitable for all audiences: students, teachers, users, and developers.

---

## Directory Structure

```
docs-new/
├── INDEX.md                      # Master navigation (start here!)
│
├── user/                         # End-user documentation
│   ├── 00-user-manual.md        # Complete user guide
│   ├── 01-programming-guide.md  # All six languages tutorial
│   ├── 02-quick-reference.md    # Command cheat sheets
│   └── 03-faq.md                # Frequently asked questions
│
├── student/                      # Student learning materials
│   ├── 00-workbook.md           # Progressive lessons
│   ├── 01-exercises.md          # Practice problems
│   └── 02-projects.md           # Project ideas
│
├── teacher/                      # Educator resources
│   ├── 00-overview.md           # Teacher's guide
│   ├── 01-lesson-plans.md       # Ready-to-use lessons
│   ├── 02-assessments.md        # Evaluation tools
│   └── 03-classroom-mgmt.md     # Classroom strategies
│
├── installation/                 # Setup guides
│   ├── 00-quickstart.md         # Quick install
│   ├── 01-linux.md              # Linux detailed guide
│   ├── 02-macos.md              # macOS detailed guide
│   ├── 03-windows.md            # Windows detailed guide
│   └── 04-troubleshooting.md    # Common problems
│
├── developer/                    # Developer documentation
│   ├── 00-developer-guide.md    # Contributing guide
│   ├── 01-technical-reference.md # Architecture details
│   ├── 02-building.md           # Build instructions
│   └── 03-api.md                # API documentation
│
├── reference/                    # Technical references
│   ├── 00-languages.md          # Language specifications
│   ├── 01-api.md                # Programmatic interface
│   └── 02-turtle.md             # Turtle graphics reference
│
└── architecture/                 # System design
    ├── 00-overview.md           # High-level architecture
    ├── 01-executors.md          # Language executor pattern
    └── 02-ui-design.md          # UI architecture
```

---

## Quick Navigation

### I want to...

**Learn to use Time Warp IDE**
→ Start with [INDEX.md](INDEX.md) → [User Manual](user/00-user-manual.md)

**Teach with Time Warp IDE**
→ Read [Teacher's Guide](teacher/00-overview.md)

**Help students learn programming**
→ Use [Student Workbook](student/00-workbook.md)

**Install Time Warp IDE**
→ Follow [Quick Start](installation/00-quickstart.md)

**Contribute to development**
→ Read [Developer Guide](developer/00-developer-guide.md)

**Understand the architecture**
→ Check [Architecture Overview](architecture/00-overview.md)

**Look up a specific command**
→ See [Language References](reference/00-languages.md)

---

## Documentation Philosophy

This documentation follows these principles:

### Audience-Specific
- **Students**: Simple language, progressive difficulty, lots of examples
- **Teachers**: Pedagogical strategies, lesson plans, assessment tools
- **Users**: Task-oriented, practical, clear instructions
- **Developers**: Technical depth, architecture, contribution guidelines

### Professional Quality
- Written in clear, natural language (not robotic)
- Comprehensive coverage of all features
- Consistent formatting and structure
- Well-organized with clear navigation
- Regularly updated and maintained

### Accessible
- Multiple entry points for different needs
- Extensive cross-referencing
- Progressive complexity (basics → advanced)
- Real-world examples
- Troubleshooting guides

---

## Documentation Standards

### File Naming
- Numbered for reading order: `00-`, `01-`, `02-`
- Descriptive names: `user-manual.md`, not `doc1.md`
- Lowercase with hyphens: `lesson-plans.md`

### Formatting
- Markdown with GitHub Flavored Markdown extensions
- Code blocks with language specification
- Consistent heading hierarchy (H1 for title, H2 for sections)
- Tables for structured data
- Lists for steps and options

### Content Structure
```markdown
# Document Title

Brief introduction explaining purpose and audience.

---

## Table of Contents

1. [Section One](#section-one)
2. [Section Two](#section-two)

---

## Section One

Content with examples...

### Subsection

More specific content...

---

## Next Steps

Links to related documents...
```

### Code Examples
- Complete and runnable
- Commented for clarity
- Show expected output
- Cover common use cases

### Links
- Relative paths for portability
- Descriptive link text (not "click here")
- Check for broken links regularly

---

## Contributing to Documentation

### Making Changes

1. **Edit** markdown files directly
2. **Preview** using GitHub or local markdown viewer
3. **Test** all code examples
4. **Check** links and cross-references
5. **Submit** pull request

### Adding New Documents

1. **Determine** appropriate directory
2. **Follow** naming conventions
3. **Add** to relevant table of contents
4. **Cross-reference** from related docs
5. **Update** INDEX.md if needed

### Style Guide

**Voice**: Second person ("you") for user-facing, third person for technical

**Tense**: Present tense ("the program runs") not past ("the program ran")

**Active voice**: "Click the button" not "the button should be clicked"

**Clarity**: Short sentences, simple words when possible

**Examples**: Practical, relevant to audience

### Review Process

All documentation changes should:
- [ ] Be technically accurate
- [ ] Follow style guide
- [ ] Include examples where appropriate
- [ ] Link to related content
- [ ] Be free of typos and grammatical errors

---

## Building Documentation

### Local Preview

**Using Python**:
```bash
python -m http.server 8000
# Navigate to http://localhost:8000
```

**Using VS Code**:
- Install "Markdown Preview Enhanced" extension
- Right-click markdown file → "Markdown Preview Enhanced: Open Preview"

### Generating PDF

```bash
# Requires pandoc
pandoc docs-new/user/00-user-manual.md -o user-manual.pdf
```

### Generating Website

```bash
# Using MkDocs (example)
mkdocs build
```

---

## Documentation Roadmap

### Current Status ✅
- [x] Master index
- [x] User manual
- [x] Programming guide
- [x] Teacher's guide
- [x] Student workbook
- [x] Quick start installation
- [x] Developer guide

### In Progress 🚧
- [ ] Language references (detailed)
- [ ] API documentation
- [ ] Architecture diagrams
- [ ] Video tutorials (planned)

### Planned 📋
- [ ] Printable student worksheets
- [ ] Interactive tutorials
- [ ] Translated versions (Spanish, French, etc.)
- [ ] Community cookbook
- [ ] Advanced topics guides

---

## Getting Help

**Documentation unclear?**
- Open an [issue](https://github.com/honey-badger-org/Time_Warp/issues/new)
- Suggest improvements

**Want to help improve docs?**
- Read [Contributing Guide](developer/03-contributing.md)
- Submit pull requests

**Found an error?**
- Report it immediately
- Corrections appreciated!

---

## License

Documentation is licensed under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/).

Code examples in documentation are licensed under the same license as Time Warp IDE (MIT License).

---

*Last updated: November 2025*
*Documentation version: 4.0.0*
