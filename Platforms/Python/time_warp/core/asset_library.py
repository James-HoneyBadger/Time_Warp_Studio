"""Asset library system for managing sprites, images, and sounds."""

import json
import os
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple


class AssetType(Enum):
    """Types of assets supported."""

    SPRITE = "sprite"
    IMAGE = "image"
    SOUND = "sound"
    TILESET = "tileset"
    ANIMATION = "animation"


@dataclass
class Asset:
    """Represents a single asset."""

    name: str
    asset_type: AssetType
    file_path: Path
    description: str = ""
    tags: List[str] = None
    width: int = 0
    height: int = 0
    duration: float = 0.0  # For sounds/animations
    frames: int = 1  # For animations

    def __post_init__(self):
        if self.tags is None:
            self.tags = []


@dataclass
class AssetLibraryInfo:
    """Metadata about an asset library."""

    name: str
    version: str
    author: str
    description: str
    asset_count: int
    last_updated: str


class AssetLibrary:
    """Manages game assets (sprites, images, sounds)."""

    def __init__(self, library_path: Optional[Path] = None):
        """Initialize asset library."""
        if library_path is None:
            library_path = Path.home() / ".time_warp" / "assets"

        self.library_path = Path(library_path)
        self.library_path.mkdir(parents=True, exist_ok=True)

        self.assets: Dict[str, Asset] = {}
        self._builtin_assets = self._initialize_builtin_assets()
        self._load_library_metadata()

    def _initialize_builtin_assets(self) -> Dict[str, Asset]:
        """Initialize built-in asset library."""
        assets = {}

        # Built-in sprites (placeholder names - actual files would be created)
        builtin = {
            "player": {
                "type": AssetType.SPRITE,
                "desc": "Default player character sprite",
                "tags": ["character", "player", "animated"],
            },
            "enemy": {
                "type": AssetType.SPRITE,
                "desc": "Generic enemy sprite",
                "tags": ["character", "enemy", "game"],
            },
            "coin": {
                "type": AssetType.SPRITE,
                "desc": "Collectible coin sprite",
                "tags": ["item", "collectible", "game"],
            },
            "background_forest": {
                "type": AssetType.IMAGE,
                "desc": "Forest background image",
                "tags": ["background", "nature", "outdoor"],
            },
            "background_space": {
                "type": AssetType.IMAGE,
                "desc": "Space background image",
                "tags": ["background", "space", "sci-fi"],
            },
            "tileset_grass": {
                "type": AssetType.TILESET,
                "desc": "Grass and earth tileset",
                "tags": ["tileset", "terrain", "nature"],
            },
            "jump_sound": {
                "type": AssetType.SOUND,
                "desc": "Jump sound effect",
                "tags": ["sound", "effect", "game"],
            },
            "coin_pickup": {
                "type": AssetType.SOUND,
                "desc": "Coin collection sound",
                "tags": ["sound", "effect", "reward"],
            },
            "background_music": {
                "type": AssetType.SOUND,
                "desc": "Background game music",
                "tags": ["sound", "music", "ambient"],
            },
        }

        for asset_name, asset_info in builtin.items():
            assets[asset_name] = Asset(
                name=asset_name,
                asset_type=asset_info["type"],
                file_path=self.library_path / asset_name,
                description=asset_info["desc"],
                tags=asset_info["tags"],
            )

        return assets

    def _load_library_metadata(self):
        """Load library metadata."""
        metadata_file = self.library_path / "library.json"
        if metadata_file.exists():
            try:
                with open(metadata_file) as f:
                    metadata = json.load(f)
                    # Could load additional metadata here
            except Exception:
                pass

    def import_asset(
        self,
        file_path: Path,
        name: Optional[str] = None,
        asset_type: Optional[AssetType] = None,
        tags: Optional[List[str]] = None,
    ) -> Asset:
        """Import an asset from file."""
        file_path = Path(file_path)

        if not file_path.exists():
            raise FileNotFoundError(f"Asset file not found: {file_path}")

        # Determine name if not provided
        if name is None:
            name = file_path.stem

        # Determine type if not provided
        if asset_type is None:
            asset_type = self._infer_asset_type(file_path)

        # Copy file to library
        dest_path = self.library_path / f"{name}{file_path.suffix}"
        if not dest_path.exists():
            import shutil

            shutil.copy2(file_path, dest_path)

        # Create asset entry
        asset = Asset(
            name=name,
            asset_type=asset_type,
            file_path=dest_path,
            tags=tags or [],
        )

        # Get dimensions for images/sprites
        if asset_type in [AssetType.SPRITE, AssetType.IMAGE]:
            asset.width, asset.height = self._get_image_dimensions(dest_path)

        # Get duration for sounds
        elif asset_type == AssetType.SOUND:
            asset.duration = self._get_audio_duration(dest_path)

        self.assets[name] = asset
        self._save_asset_metadata(asset)

        return asset

    def get_asset(self, name: str) -> Optional[Asset]:
        """Get an asset by name."""
        # Check user assets first
        if name in self.assets:
            return self.assets[name]

        # Then check builtin
        if name in self._builtin_assets:
            return self._builtin_assets[name]

        return None

    def get_assets_by_type(self, asset_type: AssetType) -> List[Asset]:
        """Get all assets of a specific type."""
        all_assets = {**self.assets, **self._builtin_assets}
        return [a for a in all_assets.values() if a.asset_type == asset_type]

    def get_assets_by_tag(self, tag: str) -> List[Asset]:
        """Get all assets with a specific tag."""
        all_assets = {**self.assets, **self._builtin_assets}
        return [a for a in all_assets.values() if tag in a.tags]

    def search_assets(self, query: str) -> List[Asset]:
        """Search assets by name or description."""
        all_assets = {**self.assets, **self._builtin_assets}
        query_lower = query.lower()

        results = []
        for asset in all_assets.values():
            if (
                query_lower in asset.name.lower()
                or query_lower in asset.description.lower()
                or any(query_lower in tag for tag in asset.tags)
            ):
                results.append(asset)

        return results

    def delete_asset(self, name: str) -> bool:
        """Delete an asset."""
        if name in self.assets:
            asset = self.assets[name]
            if asset.file_path.exists():
                asset.file_path.unlink()
            del self.assets[name]
            return True
        return False

    def list_all(self) -> List[Asset]:
        """List all available assets."""
        return list(self.assets.values()) + list(self._builtin_assets.values())

    def get_library_info(self) -> AssetLibraryInfo:
        """Get library information."""
        return AssetLibraryInfo(
            name="Time Warp Asset Library",
            version="1.0.0",
            author="Time Warp Studio",
            description="Built-in and user assets for games and graphics",
            asset_count=len(self.assets) + len(self._builtin_assets),
            last_updated=self._get_last_modified(),
        )

    def _infer_asset_type(self, file_path: Path) -> AssetType:
        """Infer asset type from file extension."""
        suffix = file_path.suffix.lower()

        if suffix in [".png", ".jpg", ".jpeg", ".gif", ".bmp"]:
            if file_path.stem.endswith("_tileset"):
                return AssetType.TILESET
            elif file_path.stem.endswith("_animation"):
                return AssetType.ANIMATION
            else:
                return AssetType.SPRITE

        elif suffix in [".wav", ".mp3", ".ogg", ".flac"]:
            return AssetType.SOUND

        else:
            return AssetType.IMAGE

    def _get_image_dimensions(self, file_path: Path) -> Tuple[int, int]:
        """Get image dimensions."""
        try:
            from PIL import Image

            img = Image.open(file_path)
            return img.size
        except Exception:
            return (0, 0)

    def _get_audio_duration(self, file_path: Path) -> float:
        """Get audio file duration in seconds."""
        try:
            # Try using librosa if available
            try:
                import librosa

                y, sr = librosa.load(file_path)
                return librosa.get_duration(y=y, sr=sr)
            except ImportError:
                pass

            # Fallback: try pydub
            try:
                from pydub import AudioSegment

                audio = AudioSegment.from_file(str(file_path))
                return len(audio) / 1000.0
            except ImportError:
                pass

        except Exception:
            pass

        return 0.0

    def _save_asset_metadata(self, asset: Asset):
        """Save asset metadata to JSON."""
        metadata_file = self.library_path / f"{asset.name}_metadata.json"

        metadata = {
            "name": asset.name,
            "type": asset.asset_type.value,
            "description": asset.description,
            "tags": asset.tags,
            "width": asset.width,
            "height": asset.height,
            "duration": asset.duration,
            "frames": asset.frames,
        }

        try:
            with open(metadata_file, "w") as f:
                json.dump(metadata, f, indent=2)
        except Exception:
            pass

    def _get_last_modified(self) -> str:
        """Get library last modified time."""
        try:
            times = [os.path.getmtime(f) for f in self.library_path.glob("*")]
            if times:
                from datetime import datetime

                return datetime.fromtimestamp(max(times)).isoformat()
        except Exception:
            pass
        return "Unknown"

    def export_manifest(self) -> Dict:
        """Export library manifest as dictionary."""
        manifest = {"library": self.get_library_info().__dict__, "assets": []}

        for asset in self.list_all():
            manifest["assets"].append(
                {
                    "name": asset.name,
                    "type": asset.asset_type.value,
                    "description": asset.description,
                    "tags": asset.tags,
                    "file_path": str(asset.file_path),
                }
            )

        return manifest


class SpriteAnimator:
    """Manages sprite animation sequences."""

    def __init__(
        self,
        sprite_asset: Asset,
        frame_count: int,
        frame_width: int,
        frame_height: int,
    ):
        """Initialize sprite animator."""
        self.sprite = sprite_asset
        self.frame_count = frame_count
        self.frame_width = frame_width
        self.frame_height = frame_height
        self.current_frame = 0
        self.is_playing = False
        self.fps = 10

    def play(self):
        """Start animation."""
        self.is_playing = True
        self.current_frame = 0

    def stop(self):
        """Stop animation."""
        self.is_playing = False

    def pause(self):
        """Pause animation."""
        self.is_playing = False

    def resume(self):
        """Resume animation."""
        self.is_playing = True

    def next_frame(self):
        """Move to next frame."""
        if self.is_playing:
            self.current_frame = (self.current_frame + 1) % self.frame_count

    def get_current_frame_rect(self) -> Tuple[int, int, int, int]:
        """Get rectangle of current frame (x, y, w, h)."""
        x = (
            self.current_frame % (self.sprite.width // self.frame_width)
        ) * self.frame_width
        y = (
            self.current_frame // (self.sprite.width // self.frame_width)
        ) * self.frame_height
        return (x, y, self.frame_width, self.frame_height)
