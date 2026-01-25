"""Theme editor and preset manager for Time Warp Studio.

Create, customize, and share editor themes.
"""

import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Dict, List, Optional


@dataclass
class ThemeColors:
    """Color scheme for a theme."""

    # Editor colors
    background: str
    foreground: str
    selection: str
    line_number: str
    cursor: str

    # Syntax highlighting
    keyword: str
    string: str
    number: str
    comment: str
    operator: str
    function: str
    variable: str

    # UI elements
    accent: str
    error: str
    warning: str
    success: str


@dataclass
class Theme:
    """A complete theme definition."""

    id: str
    name: str
    description: str
    author: str
    colors: ThemeColors
    font_family: str = "Monospace"
    font_size: int = 12
    line_height: float = 1.5
    custom: bool = False  # User-created theme

    def to_dict(self) -> dict:
        """Convert to dictionary."""
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "author": self.author,
            "colors": asdict(self.colors),
            "font_family": self.font_family,
            "font_size": self.font_size,
            "line_height": self.line_height,
            "custom": self.custom,
        }

    @classmethod
    def from_dict(cls, data: dict) -> "Theme":
        """Create from dictionary."""
        return cls(
            id=data["id"],
            name=data["name"],
            description=data["description"],
            author=data["author"],
            colors=ThemeColors(**data["colors"]),
            font_family=data.get("font_family", "Monospace"),
            font_size=data.get("font_size", 12),
            line_height=data.get("line_height", 1.5),
            custom=data.get("custom", False),
        )


class ThemeManager:
    """Manage themes and presets."""

    BUILTIN_THEMES = {
        "dracula": {
            "name": "Dracula",
            "description": "Dark theme with vibrant colors",
            "author": "Zeno Rocha",
            "colors": {
                "background": "#282a36",
                "foreground": "#f8f8f2",
                "selection": "#44475a",
                "line_number": "#6272a4",
                "cursor": "#f8f8f2",
                "keyword": "#ff79c6",
                "string": "#f1fa8c",
                "number": "#bd93f9",
                "comment": "#6272a4",
                "operator": "#ff79c6",
                "function": "#50fa7b",
                "variable": "#8be9fd",
                "accent": "#ff79c6",
                "error": "#ff5555",
                "warning": "#ffb86c",
                "success": "#50fa7b",
            },
        },
        "solarized_dark": {
            "name": "Solarized Dark",
            "description": "Precision colors for machines and people",
            "author": "Ethan Schoonover",
            "colors": {
                "background": "#002b36",
                "foreground": "#839496",
                "selection": "#073642",
                "line_number": "#586e75",
                "cursor": "#839496",
                "keyword": "#859900",
                "string": "#2aa198",
                "number": "#d33682",
                "comment": "#586e75",
                "operator": "#859900",
                "function": "#268bd2",
                "variable": "#2aa198",
                "accent": "#268bd2",
                "error": "#dc322f",
                "warning": "#b58900",
                "success": "#859900",
            },
        },
        "light": {
            "name": "Light",
            "description": "Clean light theme",
            "author": "Time Warp",
            "colors": {
                "background": "#ffffff",
                "foreground": "#1e1e1e",
                "selection": "#e0e0ff",
                "line_number": "#999999",
                "cursor": "#1e1e1e",
                "keyword": "#0000ff",
                "string": "#a31515",
                "number": "#098658",
                "comment": "#008000",
                "operator": "#0000ff",
                "function": "#6f42c1",
                "variable": "#001080",
                "accent": "#0066cc",
                "error": "#ff0000",
                "warning": "#ff8800",
                "success": "#008000",
            },
        },
    }

    def __init__(self, themes_dir: Optional[Path] = None):
        self.themes: Dict[str, Theme] = {}
        self.themes_dir = themes_dir or Path.home() / ".time_warp" / "themes"
        self.current_theme_id = "dracula"
        self._load_builtin_themes()
        self._load_custom_themes()

    def _load_builtin_themes(self) -> None:
        """Load built-in themes."""
        for theme_id, data in self.BUILTIN_THEMES.items():
            theme = Theme(
                id=theme_id,
                name=data["name"],
                description=data["description"],
                author=data["author"],
                colors=ThemeColors(**data["colors"]),
                custom=False,
            )
            self.themes[theme_id] = theme

    def _load_custom_themes(self) -> None:
        """Load user-created themes."""
        if not self.themes_dir.exists():
            return

        for theme_file in self.themes_dir.glob("*.json"):
            try:
                data = json.loads(theme_file.read_text())
                theme = Theme.from_dict(data)
                self.themes[theme.id] = theme
            except Exception:
                pass  # Skip invalid themes

    def save_custom_theme(self, theme: Theme) -> bool:
        """Save a custom theme."""
        if not theme.custom:
            return False

        self.themes_dir.mkdir(parents=True, exist_ok=True)

        try:
            theme_file = self.themes_dir / f"{theme.id}.json"
            theme_file.write_text(json.dumps(theme.to_dict(), indent=2))
            self.themes[theme.id] = theme
            return True
        except Exception:
            return False

    def create_theme(
        self,
        name: str,
        colors: ThemeColors,
        base_theme_id: str = "dracula",
    ) -> Theme:
        """Create a new custom theme based on existing theme."""
        theme_id = f"custom_{name.lower().replace(' ', '_')}"

        theme = Theme(
            id=theme_id,
            name=name,
            description=f"Custom theme based on {base_theme_id}",
            author="User",
            colors=colors,
            custom=True,
        )

        self.save_custom_theme(theme)
        return theme

    def get_theme(self, theme_id: str) -> Optional[Theme]:
        """Get a theme by ID."""
        return self.themes.get(theme_id)

    def set_current_theme(self, theme_id: str) -> bool:
        """Set current theme."""
        if theme_id in self.themes:
            self.current_theme_id = theme_id
            return True
        return False

    def get_current_theme(self) -> Theme:
        """Get currently active theme."""
        theme = self.themes.get(self.current_theme_id)
        if not theme:
            theme = self.themes["dracula"]
        return theme

    def list_themes(self, include_custom: bool = True) -> List[Theme]:
        """List available themes."""
        themes = list(self.themes.values())

        if not include_custom:
            themes = [t for t in themes if not t.custom]

        return sorted(themes, key=lambda t: t.name)

    def get_color_palette(self) -> dict:
        """Get color palette for quick access."""
        theme = self.get_current_theme()
        return asdict(theme.colors)

    def clone_theme(self, base_theme_id: str, new_name: str) -> Optional[Theme]:
        """Clone a theme."""
        base = self.get_theme(base_theme_id)
        if not base:
            return None

        # Deep copy colors
        new_colors = ThemeColors(**asdict(base.colors))

        return self.create_theme(new_name, new_colors, base_theme_id)
