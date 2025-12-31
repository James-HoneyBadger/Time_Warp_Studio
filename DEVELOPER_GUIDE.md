# Developer Guide: New Features Implementation

## Quick Start for Integration

All feature modules are ready to import and use immediately:

```python
# Import any of the five new features
from time_warp.core.syntax_validator import SyntaxValidator
from time_warp.core.project_templates import TemplateLibrary
from time_warp.core.debugger import CodeDebugger
from time_warp.core.language_comparator import MultiLanguageComparator
from time_warp.core.asset_library import AssetLibrary
```

---

## Module Inventory

### 1. `syntax_validator.py` - Real-Time Code Validation

**Main Classes:**
- `SyntaxValidator` - Main validator interface
- `SyntaxIssue` - Issue data class (line, column, message, severity)
- `SeverityLevel` - Enum (ERROR, WARNING, INFO)

**Key Method:**
```python
def validate(code: str, language: Language) -> List[SyntaxIssue]
```

**Language Support:** BASIC, LOGO, PILOT, PASCAL, C, FORTH, PROLOG

**Usage in UI:**
```python
# In editor.py: on_text_changed event
issues = validator.validate(editor.toPlainText(), current_language)
for issue in issues:
    display_syntax_error(issue.line, issue.message)
```

---

### 2. `project_templates.py` - Template Library

**Main Classes:**
- `TemplateLibrary` - Static template manager (no instantiation needed)
- `Template` - Template data class
- `TemplateCategory` - Enum (GAME, DATA_VIZ, ROBOTICS, ART, LEARNING, DEMO)

**Key Methods:**
```python
TemplateLibrary.get_all() -> Dict[str, Template]
TemplateLibrary.get_by_category(category) -> List[Template]
TemplateLibrary.get_for_beginner() -> List[Template]
TemplateLibrary.search(query) -> List[Template]
```

**Templates Available:** 8 (see FEATURES_COMPLETE.md for list)

**Usage in UI:**
```python
# In new_project_dialog.py
templates = TemplateLibrary.get_by_category(TemplateCategory.GAME)
# Display in combo box or tree widget
# On selection: editor.setText(template.code)
```

---

### 3. `debugger.py` - Execution Timeline Debugger

**Main Classes:**
- `ExecutionTimeline` - Records execution frames
- `ExecutionFrame` - Single step data (line, variables, state)
- `ExecutionState` - Enum (STOPPED, RUNNING, PAUSED, FINISHED, ERROR)
- `CodeDebugger` - High-level debugger interface

**Key Methods:**
```python
timeline.record_frame(line, line_content, variables, stack_depth)
timeline.set_breakpoint(line)
timeline.step_forward()  # Move to next frame
timeline.step_backward()  # Move to previous frame
timeline.get_variable_history(var_name) -> List[VariableSnapshot]
timeline.get_current_frame() -> ExecutionFrame
```

**Usage in UI:**
```python
# In debug_panel.py
debugger = CodeDebugger()
debugger.debug_code(editor.toPlainText())
timeline = debugger.get_timeline()

# Display frames in list view
for frame in timeline.frames:
    add_to_timeline_view(frame.line, frame.variables)

# Step buttons
step_forward_button.clicked.connect(timeline.step_forward)
```

---

### 4. `language_comparator.py` - Multi-Language Comparison

**Main Classes:**
- `MultiLanguageComparator` - Comparison engine
- `LanguageComparison` - Comparison result
- `ComparisonRenderer` - Renders results
- `ComparisonMode` - Enum (SYNTAX, EXECUTION, TIMING, VISUALIZATION)

**Key Methods:**
```python
comparator.get_builtin_pairs() -> Dict[str, Tuple[str, str]]
comparator.compare(lang1, code1, lang2, code2, mode) -> LanguageComparison
comparator.get_comparison_metrics(comparison) -> Dict[str, any]
ComparisonRenderer.render_html(comparison) -> str
ComparisonRenderer.render_text(comparison) -> str
```

**Built-in Pairs:** 5 (Hello World, Loops, Squares, Calculations, Conditionals)

**Usage in UI:**
```python
# In comparison_dialog.py
comparator = MultiLanguageComparator()
comparison = comparator.compare(lang1, code1, lang2, code2)
html = ComparisonRenderer.render_html(comparison)
# Display in QTextBrowser or save to file
```

---

### 5. `asset_library.py` - Asset Management

**Main Classes:**
- `AssetLibrary` - Asset manager
- `Asset` - Asset data class
- `AssetType` - Enum (SPRITE, IMAGE, SOUND, TILESET, ANIMATION)
- `SpriteAnimator` - Animation sequencing

**Key Methods:**
```python
library.import_asset(file_path, name, asset_type, tags) -> Asset
library.get_asset(name) -> Asset
library.get_assets_by_type(asset_type) -> List[Asset]
library.get_assets_by_tag(tag) -> List[Asset]
library.search_assets(query) -> List[Asset]
library.list_all() -> List[Asset]
```

**Built-in Assets:** 9 (player, enemy, coin, backgrounds, sounds)

**Usage in UI:**
```python
# In asset_manager_panel.py
library = AssetLibrary()
assets = library.get_assets_by_type(AssetType.SPRITE)
for asset in assets:
    add_to_asset_list(asset.name, asset.description)

# On import
library.import_asset(selected_file.path)
```

---

## Integration Checklist

### Phase 1: Core Integration (Week 1)
- [ ] Hook syntax validator to editor text changes
  - File: `ui/editor.py`
  - Action: Connect `textChanged` signal to validation
  - UI Effect: Show red underlines on error lines
  
- [ ] Add templates to "New Project" dialog
  - File: `ui/dialogs.py` or new `ui/new_project_dialog.py`
  - Action: Populate templates from `TemplateLibrary`
  - UI Effect: Show template browser on File > New

### Phase 2: Enhancement (Week 2)
- [ ] Create asset manager panel
  - File: new `ui/asset_manager.py`
  - Action: Integrate `AssetLibrary`
  - UI Effect: New dockable panel for asset management
  
- [ ] Add language comparison tool
  - File: new `ui/comparison_dialog.py`
  - Action: Create dialog with `MultiLanguageComparator`
  - UI Effect: Tools > Compare Languages menu item

### Phase 3: Advanced (Week 3)
- [ ] Enhance debug panel with timeline
  - File: `ui/debug_panel.py` (extend existing)
  - Action: Integrate `CodeDebugger`
  - UI Effect: Timeline scrubber, frame list, variable inspector

---

## File Location Map

```
Platforms/Python/
├── time_warp/
│   ├── core/
│   │   ├── syntax_validator.py        ← Real-time validation
│   │   ├── project_templates.py       ← Template library
│   │   ├── debugger.py                ← Timeline debugger
│   │   ├── language_comparator.py     ← Language comparison
│   │   ├── asset_library.py           ← Asset management
│   │   └── interpreter.py             ← Existing (no changes)
│   ├── ui/
│   │   ├── editor.py                  ← Update for syntax validation
│   │   ├── main_window.py             ← Update for new menu items
│   │   ├── dialogs.py                 ← Update new project dialog
│   │   ├── debug_panel.py             ← Enhance with debugger
│   │   └── (new) new_project_dialog.py ← Optional: dedicated templates UI
│   │   └── (new) comparison_dialog.py  ← Optional: language comparison
│   │   └── (new) asset_manager.py      ← Optional: asset management
│   └── tests/
│       └── test_new_features.py       ← Integration tests
```

---

## Common Integration Patterns

### Pattern 1: Simple Signal Connection
```python
# In __init__ or setup method:
self.editor.textChanged.connect(self.on_text_changed)

def on_text_changed(self):
    from time_warp.core.syntax_validator import SyntaxValidator
    validator = SyntaxValidator()
    issues = validator.validate(self.editor.toPlainText(), self.language)
    self.display_errors(issues)
```

### Pattern 2: Dialog Integration
```python
# In menu handler:
from time_warp.core.project_templates import TemplateLibrary

dialog = QDialog(self)
# Populate with templates
templates = TemplateLibrary.get_all()
# Add to UI, get selection
selected = show_template_browser(templates)
# Apply template
self.editor.setText(selected.code)
```

### Pattern 3: Dockable Panel
```python
# In main_window.py __init__:
from time_warp.core.asset_library import AssetLibrary

asset_panel = QDockWidget("Assets")
asset_manager = AssetLibraryWidget(AssetLibrary())
asset_panel.setWidget(asset_manager)
self.addDockWidget(Qt.RightDockWidgetArea, asset_panel)
```

---

## Performance Considerations

### Syntax Validator
- Caches results (max 1000 entries)
- Non-blocking validation
- CPU-efficient regex patterns
- **Recommendation**: Run on `textChanged` with 500ms debounce

### Debugger
- Records frame data efficiently
- Minimal memory overhead
- **Recommendation**: Only enable when user clicks debug button

### Asset Library
- Lazy loads metadata
- Caches image dimensions
- **Recommendation**: Load library once on startup, reuse instance

### Comparator
- Executes code to compare outputs
- **Recommendation**: Run in background thread for large programs

---

## Testing Your Integration

### Unit Tests
```bash
# Run feature tests
python test_new_features.py

# Should see: ✅ ALL TESTS PASSED!
```

### Manual Testing Checklist
- [ ] Syntax validator shows errors while typing
- [ ] Templates load in new project dialog
- [ ] Assets appear in asset manager
- [ ] Language comparison produces correct output
- [ ] Debugger steps through code correctly

---

## Troubleshooting

### Import Errors
**Problem**: `ImportError: cannot import name 'X'`
- **Solution**: Check correct class names in module docstrings
- Module classes are exported directly, no sub-imports needed

### Empty Results
**Problem**: Templates/assets return empty list
- **Solution**: Call `TemplateLibrary.initialize()` or `AssetLibrary()` first
- Lazy initialization happens on first access

### Performance Issues
**Problem**: Syntax validation slows down editor
- **Solution**: Add debounce to validation calls
- Don't validate on every keystroke, use 300-500ms debounce

---

## Future Enhancement Ideas

1. **Custom Validators**: Allow users to write custom syntax rules
2. **Template Marketplace**: Community-contributed templates
3. **Advanced Debugging**: Conditional breakpoints, watches
4. **Asset Sharing**: Export/share custom assets
5. **Performance Profiling**: Show slow lines

---

## Questions?

See [IMPLEMENTATION_COMPLETE.md](IMPLEMENTATION_COMPLETE.md) for integration guide and usage examples.

---

**Ready to integrate?** Start with Phase 1 checklist above! 🚀
