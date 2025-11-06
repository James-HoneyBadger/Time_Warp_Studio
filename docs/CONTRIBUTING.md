# Contributing to Time Warp IDE

**🚀 Welcome to the Time Warp Community!**

Time Warp IDE is built by an amazing community of educators, students, developers, and programming enthusiasts from around the world. Whether you're fixing a bug, adding a feature, improving documentation, or creating educational content, your contributions help make programming more accessible and enjoyable for everyone.

---

## 🌟 Ways to Contribute

### 💻 **Code Contributions**
- **🐛 Bug Fixes**: Help us squash bugs and improve reliability
- **✨ New Features**: Implement new TempleCode commands, IDE features, or educational tools
- **⚡ Performance**: Optimize algorithms, reduce memory usage, improve startup time
- **🔧 Maintenance**: Code cleanup, refactoring, and technical debt reduction
- **🧪 Testing**: Add test cases, improve test coverage, create automated tests

### 📚 **Documentation & Content**
- **📖 User Guides**: Improve installation instructions, tutorials, and usage examples
- **🎓 Educational Materials**: Create lesson plans, activities, and curriculum content
- **🔧 Developer Docs**: API documentation, architecture guides, and code examples
- **🌍 Translation**: Translate documentation and interface text to other languages
- **📝 Blog Posts**: Share tutorials, tips, and success stories

### 🎨 **Design & User Experience**
- **🖼️ User Interface**: Improve IDE layouts, color schemes, and usability
- **🎯 User Experience**: Streamline workflows, reduce complexity, enhance accessibility
- **📱 Responsive Design**: Improve mobile and tablet experiences for web implementation
- **♿ Accessibility**: Ensure compatibility with screen readers and assistive technologies
- **🎨 Graphics**: Create icons, illustrations, and visual aids for documentation

### 🏫 **Educational & Community**
- **👨‍🏫 Curriculum Development**: Create structured learning paths and assessment tools
- **🎮 Example Programs**: Write engaging demos, games, and educational simulations
- **💬 Community Support**: Help other users in discussions, forums, and chat
- **🎪 Event Organization**: Organize hackathons, workshops, and educational sessions
- **📢 Advocacy**: Share Time Warp with educators, schools, and programming communities

## Code of Conduct

This project follows a simple code of conduct:

- **Be Respectful**: Treat all contributors with respect and courtesy
- **Be Constructive**: Provide helpful, actionable feedback
- **Be Inclusive**: Welcome contributors of all skill levels and backgrounds
- **Be Patient**: Remember that everyone is learning
- **Be Professional**: Keep discussions focused on the project

Violations should be reported to james@honey-badger.org.

## Getting Started

### Ways to Contribute

- **Code**: Fix bugs, add features, improve performance
- **Documentation**: Improve guides, fix typos, add examples
- **Testing**: Write tests, report bugs, verify fixes
- **Examples**: Create educational programs demonstrating features
- **Translation**: Translate documentation (future)
- **Community**: Help users, answer questions, share knowledge

### Quick Contribution Checklist

- [ ] Fork the repository
- [ ] Create a feature branch
- [ ] Make your changes
- [ ] Test your changes
- [ ] Update documentation
- [ ] Submit a pull request

## Development Setup

### Python Implementation

```bash
# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Python

# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
pip install pytest pytest-cov pytest-mock  # Dev dependencies

# Run IDE
python time_warp_ide.py

# Run tests
pytest tests/ -v
```

### Rust Implementation

```bash
# Clone repository
cd Time_Warp/Time_Warp_Rust

# Build and run
cargo run

# Run tests
cargo test

# Build release
cargo build --release

# Check code
cargo clippy
cargo fmt --check
```

## Project Structure

```
Time_Warp/
├── Time_Warp_Python/       # Python implementation
│   ├── time_warp/          # Main package
│   │   ├── core/           # Interpreter engine
│   │   ├── languages/      # TempleCode executor
│   │   ├── graphics/       # Turtle graphics
│   │   ├── ui/             # PySide6 UI
│   │   └── utils/          # Utilities
│   ├── tests/              # Test suite
│   ├── examples/           # Example programs
│   └── docs/               # Documentation
│
├── Time_Warp_Rust/         # Rust implementation
│   ├── src/                # Source code
│   │   ├── interpreter/    # Core interpreter
│   │   ├── languages/      # Language modules
│   │   ├── graphics/       # Turtle & canvas
│   │   ├── ui/             # egui UI
│   │   └── compiler/       # TempleCode compiler
│   ├── tests/              # Rust tests
│   └── docs/               # Documentation
│
├── examples/               # Shared examples (33)
├── docs/                   # Project-wide docs
└── .github/                # GitHub config
```

## Contribution Workflow

### 1. Fork and Clone

```bash
# Fork on GitHub, then:
git clone https://github.com/YOUR_USERNAME/Time_Warp.git
cd Time_Warp
git remote add upstream https://github.com/James-HoneyBadger/Time_Warp.git
```

### 2. Create a Branch

```bash
# Create a feature branch
git checkout -b feature/your-feature-name

# Or for bug fixes
git checkout -b fix/bug-description
```

Branch naming conventions:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation updates
- `refactor/` - Code refactoring
- `test/` - Test additions/improvements

### 3. Make Changes

- Write clear, concise code
- Follow existing code style
- Add tests for new features
- Update documentation
- Test your changes thoroughly

### 4. Commit Changes

```bash
# Stage changes
git add .

# Commit with descriptive message
git commit -m "feat: Add procedure parameter validation

- Validate parameter count matches definition
- Add error messages for mismatch
- Update tests for edge cases"
```

Commit message format:
- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation changes
- `test:` - Test changes
- `refactor:` - Code refactoring
- `chore:` - Maintenance tasks

### 5. Push and Create PR

```bash
# Push to your fork
git push origin feature/your-feature-name

# Create pull request on GitHub
```

## Coding Standards

### Python Code Style

- **PEP 8**: Follow Python style guide
- **Type Hints**: Use type annotations
- **Docstrings**: Document all public functions/classes
- **Line Length**: 100 characters maximum
- **Imports**: Organized (stdlib, third-party, local)

```python
def execute_command(self, command: str) -> str:
    """Execute a single command and return output.
    
    Args:
        command: The command string to execute
        
    Returns:
        Command output as string
        
    Raises:
        ValueError: If command is invalid
    """
    # Implementation
```

### Rust Code Style

- **rustfmt**: Run `cargo fmt` before committing
- **clippy**: No clippy warnings (`cargo clippy`)
- **Documentation**: Doc comments for public items
- **Error Handling**: Use `Result` types, avoid panics
- **Naming**: snake_case functions, PascalCase types

```rust
/// Execute a command and return the output.
///
/// # Arguments
/// * `command` - The command string to execute
///
/// # Returns
/// * `Ok(String)` - Command output
/// * `Err(Error)` - Execution error
///
/// # Example
/// ```
/// let output = executor.execute("FORWARD 100")?;
/// ```
pub fn execute(&mut self, command: &str) -> Result<String> {
    // Implementation
}
```

### Shared Style Guidelines

- **Clear Names**: Use descriptive variable/function names
- **Comments**: Explain *why*, not *what*
- **Small Functions**: Keep functions focused and short
- **DRY Principle**: Don't Repeat Yourself
- **Error Messages**: Clear, actionable error messages

## Testing Guidelines

### Python Testing

```python
# Use pytest
def test_logo_procedure_with_parameters():
    """Test Logo procedures accept and use parameters correctly."""
    interp = Interpreter()
    code = """
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END
SQUARE 100
    """
    interp.load_program(code)
    output = interp.execute()
    assert len(output) > 0
```

### Rust Testing

```rust
#[test]
fn test_logo_procedure_parameters() {
    let mut interp = Interpreter::new();
    let code = r#"
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END
SQUARE 100
    "#;
    assert!(interp.load_program(code).is_ok());
    let mut turtle = TurtleState::new();
    assert!(interp.execute(&mut turtle).is_ok());
}
```

### Test Coverage

- **Unit Tests**: Test individual functions/methods
- **Integration Tests**: Test component interactions
- **Edge Cases**: Test boundary conditions
- **Error Handling**: Test error paths
- **Examples**: Ensure example programs run

Target: 80%+ code coverage for new code.

## Documentation

### What to Document

- **User-Facing**: New features, command changes
- **Developer**: API changes, architecture decisions
- **Examples**: Add example programs for new features
- **Inline**: Complex algorithms, non-obvious code

### Documentation Files to Update

- **README.md**: For major changes
- **CHANGELOG.md**: All changes
- **USER_GUIDE.md**: User-visible features
- **API docs**: For API changes
- **Code comments**: For implementation details

## Pull Request Process

### Before Submitting

- [ ] Tests pass (`pytest` or `cargo test`)
- [ ] Code formatted (`black .` or `cargo fmt`)
- [ ] No linter warnings
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Commit messages clear

### PR Description Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
Describe testing performed

## Checklist
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Code follows style guidelines
```

### Review Process

1. **Automated Checks**: CI runs tests (when configured)
2. **Code Review**: Maintainer reviews code
3. **Feedback**: Address review comments
4. **Approval**: Maintainer approves
5. **Merge**: Maintainer merges to main

### After Merge

- Delete your feature branch
- Update your fork
- Celebrate! 🎉

## Community

### Communication Channels

- **GitHub Issues**: Bug reports, feature requests
- **GitHub Discussions**: Questions, ideas, show & tell
- **Email**: james@honey-badger.org for private inquiries

### Issue Guidelines

**Bug Reports:**
```markdown
**Describe the bug**
Clear description of the bug

**To Reproduce**
Steps to reproduce:
1. Step 1
2. Step 2

**Expected behavior**
What should happen

**Environment**
- OS: [Linux/Windows/macOS]
- Version: [2.0.0]
- Implementation: [Python/Rust]
```

**Feature Requests:**
```markdown
**Feature Description**
Clear description of the feature

**Use Case**
Why is this needed?

**Proposed Solution**
How might it work?

**Alternatives**
Other approaches considered
```

### Recognition

Contributors will be:
- Listed in CHANGELOG.md
- Credited in release notes
- Thanked in commit messages

## Development Guidelines by Area

### Adding a New Turtle Command

1. Add to `time_warp/languages/templecode.py` (Python)
2. Add to `src/languages/logo/mod.rs` (Rust)
3. Update `TURTLE_GRAPHICS_REFERENCE.md`
4. Add tests
5. Add example program

### Adding a New Language Feature

1. Update interpreter core
2. Add language executor logic
3. Add tests (unit + integration)
4. Update documentation
5. Add example program

### Improving Documentation

1. Check for outdated info
2. Fix typos and clarity
3. Add missing examples
4. Ensure consistency across files
5. Test all code examples

### Performance Optimization

1. Profile before optimizing
2. Add benchmark tests
3. Document performance gains
4. Ensure correctness maintained
5. Update CHANGELOG with metrics

## Questions?

- Check existing documentation
- Search GitHub issues
- Ask in GitHub Discussions
- Email: james@honey-badger.org

Thank you for contributing to Time Warp IDE! Your efforts help students worldwide learn programming through this educational tool.

---

**Happy Coding! 🐢🚀**
