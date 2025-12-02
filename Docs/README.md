## Time Warp IDE Documentation

**Version 5.0.0** – Updated December 2, 2025

This directory contains the canonical documentation set for the Time Warp IDE project. The content is organized by audience so students, teachers, end users, and contributors can jump straight to what they need.

---

## Directory Overview

```
Docs/
├── INDEX.md          # Start here for navigation by persona
├── README.md         # This overview
├── developer/        # Contributor workflows and architecture
│   └── 00-developer-guide.md
├── installation/     # Setup instructions (Python IDE focus)
│   └── 00-quickstart.md
├── misc/             # Release notes and project history
│   └── RELEASE_NOTES.md
├── student/          # Classroom-ready student material
│   └── 00-workbook.md
├── teacher/          # Curriculum planning guidance
│   └── 00-overview.md
└── user/             # End-user manuals and references
    ├── 00-user-manual.md
    ├── 01-programming-guide.md
    ├── 02-quick-reference.md
    └── 03-faq.md
```

The `architecture/` and `reference/` folders are reserved for future deep-dive material and currently act as placeholders.

---

## Quick Navigation

- **New to Time Warp IDE?** Start at [INDEX.md](INDEX.md) and follow the persona-based links.
- **Need the user workflow?** Jump to the [User Manual](user/00-user-manual.md) and [Programming Guide](user/01-programming-guide.md).
- **Teaching a class?** Check the [Teacher Overview](teacher/00-overview.md) for lesson framing and assessments.
- **Preparing labs?** Share the [Student Workbook](student/00-workbook.md) for progressive exercises.
- **Installing the IDE?** Follow the [Quickstart](installation/00-quickstart.md) which covers Python environment setup.
- **Contributing code?** Review the [Developer Guide](developer/00-developer-guide.md) before submitting changes.

---

## Documentation Standards

- **Structure**: Files are numbered (`00-`, `01-`, etc.) to convey recommended reading order.
- **Formatting**: GitHub Flavored Markdown with descriptive headings, tables for structured data, and fenced code blocks with language tags.
- **Cross-references**: Use relative links inside the repository so documentation stays portable across forks and branches.
- **Versioning**: Each document should note significant updates when content changes for a new release.

---

## Contributing Updates

1. Edit the relevant markdown file and keep the numbering scheme intact.
2. Verify command snippets or code samples before submission.
3. Run a spell-check or markdown linter if you have one configured.
4. Update [INDEX.md](INDEX.md) when new documents are added so they appear in navigation.
5. Reference `Docs/misc/RELEASE_NOTES.md` when documenting major release changes.

For detailed development workflow expectations, read `Docs/developer/00-developer-guide.md`.

---

*Maintained for Time Warp IDE v5.0.0.*
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
*Documentation version: 5.0.0*
