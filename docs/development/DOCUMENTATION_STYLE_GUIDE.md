# Documentation Style Guide

**Time Warp IDE Project**  
**Last Updated:** November 18, 2025

---

## Purpose

This guide ensures consistency across all documentation, READMEs, guides, and comments in the Time Warp IDE project.

## Core Principles

1. **Clarity First** - Write for beginners; explain technical concepts
2. **Consistency** - Use standard formatting, terms, and structure
3. **Accuracy** - Match current implementation (Rust v3.0.0 primary)
4. **Brevity** - Be concise but complete
5. **Accessibility** - Support all skill levels appropriately

---

## Project Metadata

### Current Version
- **Version:** 3.0.0
- **Primary Implementation:** Rust (platforms/rust/)
- **License:** MIT
- **Maintainer:** James Temple <james@honey-badger.org>

### Language Name
- **Official:** TempleCode (one word, CamelCase)
- **Full Name:** "TempleCode - A unified educational programming language"

### Project Name
- **IDE:** Time Warp IDE (two words)
- **Short form:** Time Warp

---

## Document Structure Standards

### README Files

All README.md files should follow this structure:

```markdown
# [Component Name]

**[Brief one-line description]**

[Badges if applicable]

> **🎯 Context note** - Link to main docs if subcomponent

## Overview

[2-3 paragraphs explaining purpose and use case]

## Features

- Clear bullet list
- Focus on user benefits
- Not just technical specs

## Quick Start

```bash
# Minimal working example
# Step-by-step commands
```

## Documentation

Links to relevant guides

## License

MIT License - See [LICENSE](path/to/LICENSE)
```

### Technical Documents

Structure for architecture, implementation, and reference docs:

```markdown
# [Document Title]

**Document Type:** [Architecture|Reference|Guide]  
**Audience:** [Developers|Users|Educators]  
**Last Updated:** [Date]

---

## Table of Contents

- Clear hierarchical ToC
- Link to major sections

## Section Name

### Subsection

Content with examples

## Code Examples

```language
# Always include:
# 1. Comments explaining code
# 2. Expected output
# 3. Context of usage
```

## See Also

- Related documents
- External resources
```

### User Guides

Structure for educational and user-facing documentation:

```markdown
# [Guide Title]

**Target Audience:** [Students|Teachers|Beginners]  
**Estimated Time:** [Reading/completion time]  
**Prerequisites:** [What users should know]

---

## Learning Objectives

- Clear learning outcomes
- Measurable goals

## Step-by-Step Instructions

### Step 1: [Action]

1. Detailed sub-steps
2. Include screenshots/examples where helpful
3. Expected results

## Exercises

Hands-on practice opportunities

## Troubleshooting

Common issues and solutions

## Next Steps

Where to go after completing this guide
```

---

## Formatting Standards

### Headers

- **H1 (#):** Document title only (one per file)
- **H2 (##):** Major sections
- **H3 (###):** Subsections
- **H4 (####):** Rare, use sparingly

### Emphasis

- **Bold** - Important terms, UI elements, first use of key concepts
- *Italic* - Emphasis, variable names in prose
- `Code` - Commands, function names, file names, code snippets

### Lists

**Unordered:**
- Use `-` not `*` or `+`
- Capitalize first word
- End with period only if item is full sentence

**Ordered:**
1. Use for sequential steps
2. Keep numbers sequential (no skipping)
3. Indent sub-items with 2 spaces

### Code Blocks

Always specify language:

````markdown
```rust
// Rust example
fn main() {
    println!("Hello, TempleCode!");
}
```

```bash
# Shell commands
cargo build --release
```

```templecode
REM TempleCode example
PRINT "Hello World"
```
````

### Links

- **Internal:** Use relative paths: `[Link Text](../docs/guide.md)`
- **External:** Full URLs: `[Rust Lang](https://rust-lang.org)`
- **Files:** `[LICENSE](../LICENSE)`

### Tables

Use for structured data comparison:

```markdown
| Feature | Rust | Python | Web |
|---------|------|--------|-----|
| Speed | Excellent | Good | Good |
| Platform | Native | Cross | Browser |
```

---

## Terminology Standards

### Consistent Terms

| Use This | Not This |
|----------|----------|
| TempleCode | Temple Code, temple code |
| Time Warp IDE | TimeWarp, Time-Warp |
| Rust implementation | Rust version, Rust port |
| turtle graphics | Turtle Graphics, TURTLE |
| execute | run (in technical contexts) |
| command | statement (for consistency) |

### Language Keywords

- **UPPERCASE in examples:** `PRINT`, `IF`, `THEN`, `GOTO`
- **Lowercase in prose:** "the print command"
- **Code font:** Always use backticks: `PRINT`

### Component Names

- **Languages:** BASIC, PILOT, Logo (their original capitalization)
- **Framework:** egui, eframe (lowercase per Rust convention)
- **Features:** Turtle graphics, IoT integration (standard capitalization)

---

## Tone and Voice

### General Guidelines

- **Active voice:** "The interpreter executes commands" not "Commands are executed"
- **Second person for instructions:** "You can run" not "One can run"
- **Present tense:** "The program displays" not "The program will display"
- **Inclusive language:** Use "they/them" for generic users

### Audience-Specific Tone

**Students/Beginners:**
- Encouraging and supportive
- Explain technical terms
- Use analogies and examples
- "Let's try..." "You've learned..."

**Educators:**
- Professional but accessible
- Focus on pedagogical value
- Include assessment suggestions
- "Consider..." "Best practices..."

**Developers:**
- Technical and precise
- Assume programming knowledge
- Focus on implementation details
- "Note that..." "Implementation uses..."

---

## Emoji Usage

### Standard Prefixes (from interpreter output)

- ❌ Errors/exceptions
- ✅ Success confirmations
- ℹ️ Informational messages
- 🎨 Theme/UI changes
- 🚀 Execution/run events
- 🐢 Turtle graphics actions
- 📝 Input prompts

### Documentation Emojis (optional, use sparingly)

- 🎯 Key points, objectives
- 📚 Learning resources
- ⚠️ Warnings, important notes
- 💡 Tips, best practices
- 🔧 Technical/developer content
- 👨‍🏫 Educational/teacher content
- 🎓 Student content

**Rule:** No more than one emoji per header. None in body text.

---

## Version References

### When to Include Version Numbers

- **Release notes:** Always include version
- **Changelogs:** Version for each entry
- **Installation guides:** Mention current version
- **Technical specs:** Include version if API-specific

### Version Format

- **Standard:** `3.0.0` (semantic versioning)
- **In prose:** "version 3.0.0" (lowercase "version")
- **Git tags:** `v3.0.0` (with v prefix)

### Version Changelog Format

```markdown
## [3.0.0] - 2025-11-18

### Added
- New features

### Changed
- Modifications to existing features

### Deprecated
- Features marked for removal

### Removed
- Deleted features

### Fixed
- Bug fixes

### Security
- Security updates
```

---

## File Naming Conventions

### Documentation Files

- **ALL_CAPS.md** - Top-level project docs (README.md, LICENSE, CONTRIBUTING.md)
- **Title_Case.md** - Major guides (User_Guide.md, Developer_Reference.md)
- **lowercase.md** - Specific topics (architecture.md, installation.md)

### Code Files

- **Rust:** `snake_case.rs`
- **Python:** `snake_case.py`
- **Examples:** `descriptive_name.tc`, `example_name.bas`

### Platform Directories

- **Pattern:** `platforms/<platform>/`
- **Examples:** `platforms/rust/`, `platforms/python/`, `platforms/dos/`

---

## Special Sections

### Installation Instructions

Always include:

1. **Prerequisites** - What's needed before starting
2. **Quick Install** - Simplest path (single command if possible)
3. **Detailed Install** - Step-by-step with explanations
4. **Verification** - How to confirm successful install
5. **Troubleshooting** - Common issues and fixes

### Code Examples

Every code example should have:

```markdown
**Purpose:** What this example demonstrates

**Code:**
```templecode
REM Example code here
PRINT "Hello"
```

**Expected Output:**
```
Hello
```

**Explanation:** What's happening and why
```

### API Documentation

For functions/methods:

```markdown
### `function_name(param1: Type, param2: Type) -> ReturnType`

**Purpose:** Brief description

**Parameters:**
- `param1` - Description
- `param2` - Description

**Returns:** Description of return value

**Example:**
```rust
let result = function_name(value1, value2);
```

**Notes:** Special considerations or limitations
```

---

## Review Checklist

Before committing documentation:

- [ ] Spell check completed
- [ ] Grammar check completed
- [ ] All links work (relative and external)
- [ ] Code examples tested and accurate
- [ ] Version numbers current (3.0.0)
- [ ] Formatting consistent with style guide
- [ ] Appropriate audience tone
- [ ] No markdown lint errors
- [ ] File properly placed in structure
- [ ] Cross-references accurate

---

## Tools and Validation

### Recommended Tools

- **Spell check:** `.spellcheck.yml` (project config)
- **Markdown lint:** `.markdownlint.json` (project config)
- **Preview:** VS Code markdown preview or similar

### Validation Commands

```bash
# Check markdown formatting
markdownlint '**/*.md'

# Spell check
spellchecker '**/*.md'

# Find broken links
markdown-link-check '**/*.md'
```

---

## Questions or Clarifications

For documentation style questions:
- Check this guide first
- Review similar existing docs
- Ask maintainer: James Temple <james@honey-badger.org>
- Reference `.github/copilot-instructions.md` for project patterns

---

**This is a living document.** Update as the project evolves and new patterns emerge.
