"""
Time Warp Studio - Phase VII: Plugin Marketplace System

Provides:
- Plugin discovery and search
- Plugin ratings and reviews
- Installation and version management
- Developer publishing
- Marketplace analytics
"""

import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Dict, List, Optional


def utc_now() -> datetime:
    return datetime.now(timezone.utc)

# ===== ENUMS =====


class PluginCategory(Enum):
    """Plugin categories"""

    LANGUAGE = "language"
    THEME = "theme"
    TOOL = "tool"
    INTEGRATION = "integration"
    GAME = "game"
    EDUCATIONAL = "educational"
    UTILITIES = "utilities"
    GRAPHICS = "graphics"


class PublishStatus(Enum):
    """Publication status"""

    DRAFT = "draft"
    REVIEW = "review"
    APPROVED = "approved"
    PUBLISHED = "published"
    DEPRECATED = "deprecated"
    REMOVED = "removed"


# ===== DATA CLASSES =====


@dataclass
class PluginListing:
    """Marketplace plugin listing"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    author: str = ""
    version: str = "1.0.0"
    description: str = ""
    long_description: str = ""

    # Categorization
    category: PluginCategory = PluginCategory.TOOL
    tags: List[str] = field(default_factory=list)

    # Media
    icon_url: Optional[str] = None
    screenshots: List[str] = field(default_factory=list)

    # Details
    repository_url: Optional[str] = None
    documentation_url: Optional[str] = None
    homepage_url: Optional[str] = None

    # Metadata
    downloads: int = 0
    rating: float = 0.0
    review_count: int = 0

    # Status
    status: PublishStatus = PublishStatus.DRAFT
    published_at: Optional[datetime] = None
    updated_at: datetime = field(default_factory=utc_now)

    # Requirements
    min_version: str = "6.1.0"
    compatible_versions: List[str] = field(default_factory=list)


@dataclass
class PluginReview:
    """Plugin review"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    plugin_id: str = ""
    author_id: str = ""
    author_name: str = ""

    rating: int = 5  # 1-5 stars
    title: str = ""
    content: str = ""

    # Engagement
    helpful_votes: int = 0
    reported: bool = False

    # Metadata
    created_at: datetime = field(default_factory=utc_now)
    updated_at: datetime = field(default_factory=utc_now)


@dataclass
class PluginRelease:
    """Plugin release version"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    plugin_id: str = ""
    version: str = ""

    # Release info
    release_notes: str = ""
    download_url: str = ""
    file_size_bytes: int = 0
    checksum: str = ""

    # Status
    status: PublishStatus = PublishStatus.PUBLISHED
    released_at: datetime = field(default_factory=utc_now)

    # Stats
    downloads: int = 0
    error_reports: int = 0


@dataclass
class DeveloperProfile:
    """Plugin developer profile"""

    user_id: str = ""
    username: str = ""
    email: str = ""

    # Profile
    bio: str = ""
    website: Optional[str] = None
    avatar_url: Optional[str] = None

    # Stats
    total_plugins: int = 0
    total_downloads: int = 0
    average_rating: float = 0.0

    # Metadata
    joined_at: datetime = field(default_factory=utc_now)
    verified: bool = False


@dataclass
class PluginTemplate:
    """Code template for plugin creation"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    description: str = ""
    category: PluginCategory = PluginCategory.TOOL

    # Template content
    code: str = ""
    dependencies: List[str] = field(default_factory=list)
    example_manifest: Dict = field(default_factory=dict)

    # Metadata
    created_at: datetime = field(default_factory=utc_now)
    downloads: int = 0


# ===== MARKETPLACE SERVICES =====


class MarketplaceService:
    """Central marketplace management"""

    def __init__(self):
        self.listings: Dict[str, PluginListing] = {}
        self.reviews: Dict[str, List[PluginReview]] = {}
        self.releases: Dict[str, List[PluginRelease]] = {}
        self.developers: Dict[str, DeveloperProfile] = {}
        self.templates: Dict[str, PluginTemplate] = {}

    def publish_plugin(
        self,
        name: str,
        author_id: str,
        description: str,
        category: PluginCategory,
        repository_url: str,
    ) -> PluginListing:
        """Submit plugin for publishing"""
        listing = PluginListing(
            name=name,
            author=author_id,
            description=description,
            category=category,
            repository_url=repository_url,
            status=PublishStatus.REVIEW,
        )
        self.listings[listing.id] = listing
        return listing

    def approve_plugin(self, plugin_id: str) -> bool:
        """Approve plugin for marketplace"""
        listing = self.listings.get(plugin_id)
        if listing:
            listing.status = PublishStatus.APPROVED
            listing.published_at = utc_now()
            return True
        return False

    def search_plugins(
        self,
        query: str = "",
        category: Optional[PluginCategory] = None,
        min_rating: float = 0.0,
        limit: int = 20,
    ) -> List[PluginListing]:
        """Search marketplace plugins"""
        results = [
            p for p in self.listings.values() if p.status == PublishStatus.PUBLISHED
        ]

        if query:
            results = [
                p
                for p in results
                if query.lower() in p.name.lower()
                or query.lower() in p.description.lower()
            ]

        if category:
            results = [p for p in results if p.category == category]

        if min_rating > 0:
            results = [p for p in results if p.rating >= min_rating]

        # Sort by rating and downloads
        results.sort(key=lambda p: (p.rating, p.downloads), reverse=True)

        return results[:limit]

    def get_trending_plugins(self, limit: int = 10) -> List[PluginListing]:
        """Get trending plugins"""
        plugins = [
            p for p in self.listings.values() if p.status == PublishStatus.PUBLISHED
        ]

        # Sort by downloads in last week (simplified)
        plugins.sort(key=lambda p: p.downloads, reverse=True)
        return plugins[:limit]

    def submit_review(
        self,
        plugin_id: str,
        author_id: str,
        rating: int,
        title: str,
        content: str,
    ) -> PluginReview:
        """Submit plugin review"""
        review = PluginReview(
            plugin_id=plugin_id,
            author_id=author_id,
            rating=min(5, max(1, rating)),
            title=title,
            content=content,
        )

        if plugin_id not in self.reviews:
            self.reviews[plugin_id] = []

        self.reviews[plugin_id].append(review)

        # Update plugin rating
        self._update_plugin_rating(plugin_id)

        return review

    def get_plugin_reviews(self, plugin_id: str) -> List[PluginReview]:
        """Get all reviews for plugin"""
        return self.reviews.get(plugin_id, [])

    def release_version(
        self,
        plugin_id: str,
        version: str,
        release_notes: str,
        download_url: str,
    ) -> PluginRelease:
        """Release new plugin version"""
        release = PluginRelease(
            plugin_id=plugin_id,
            version=version,
            release_notes=release_notes,
            download_url=download_url,
        )

        if plugin_id not in self.releases:
            self.releases[plugin_id] = []

        self.releases[plugin_id].append(release)

        # Update listing
        listing = self.listings.get(plugin_id)
        if listing:
            listing.version = version
            listing.updated_at = utc_now()
            "installed_at": utc_now(),

        return release

    def get_plugin_releases(self, plugin_id: str) -> List[PluginRelease]:
        """Get all releases for plugin"""
        return self.releases.get(plugin_id, [])

    def register_developer(
        self, user_id: str, username: str, email: str
    ) -> DeveloperProfile:
        """Register as plugin developer"""
        profile = DeveloperProfile(user_id=user_id, username=username, email=email)
        self.developers[user_id] = profile
        return profile

    def get_developer_profile(self, user_id: str) -> Optional[DeveloperProfile]:
        """Get developer profile"""
        return self.developers.get(user_id)

    def create_template(
        self, name: str, description: str, category: PluginCategory, code: str
    ) -> PluginTemplate:
        """Create plugin template"""
        template = PluginTemplate(
            name=name, description=description, category=category, code=code
        )
        self.templates[template.id] = template
        return template

    def get_templates(
        self, category: Optional[PluginCategory] = None
    ) -> List[PluginTemplate]:
        """Get available templates"""
        templates = list(self.templates.values())
        if category:
            templates = [t for t in templates if t.category == category]
        return templates

    def _update_plugin_rating(self, plugin_id: str) -> None:
        """Update plugin rating from reviews"""
        reviews = self.reviews.get(plugin_id, [])
        listing = self.listings.get(plugin_id)

        if not reviews or not listing:
            return

        avg_rating = sum(r.rating for r in reviews) / len(reviews)
        listing.rating = round(avg_rating, 2)
        listing.review_count = len(reviews)


class InstallationService:
    """Manages plugin installation and versioning"""

    def __init__(self):
        self.installations: Dict[str, Dict] = {}
        self.version_locks: Dict[str, str] = {}

    def install_plugin(self, plugin_id: str, version: str, user_id: str) -> bool:
        """Install plugin for user"""
        key = f"{user_id}:{plugin_id}"
        self.installations[key] = {
            "plugin_id": plugin_id,
            "version": version,
            "installed_at": utc_now(),
            "enabled": True,
        }
        return True

    def uninstall_plugin(self, plugin_id: str, user_id: str) -> bool:
        """Uninstall plugin"""
        key = f"{user_id}:{plugin_id}"
        if key in self.installations:
            del self.installations[key]
            return True
        return False

    def get_installed_plugins(self, user_id: str) -> List[Dict]:
        """Get user's installed plugins"""
        return [v for k, v in self.installations.items() if k.startswith(f"{user_id}:")]

    def check_updates(self, plugin_id: str) -> Optional[str]:
        """Check if plugin update available"""
        # Would check marketplace for newer version
        return None


class CollaborationTemplateLibrary:
    """Library of collaboration templates"""

    def __init__(self):
        self.templates: Dict[str, Dict] = {
            "pair-programming": {
                "name": "Pair Programming",
                "description": "Real-time co-editing session",
                "features": ["real-time-sync", "cursor-tracking", "comments"],
            },
            "code-review": {
                "name": "Code Review",
                "description": "Structured code review workflow",
                "features": ["line-comments", "suggestions", "approvals"],
            },
            "teaching-session": {
                "name": "Teaching Session",
                "description": "Instructor-led learning session",
                "features": ["screen-share", "control-pass", "recordings"],
            },
            "group-challenge": {
                "name": "Group Challenge",
                "description": "Team coding challenge",
                "features": ["shared-canvas", "scoreboard", "time-tracking"],
            },
        }

    def get_template(self, template_id: str) -> Optional[Dict]:
        """Get collaboration template"""
        return self.templates.get(template_id)

    def list_templates(self) -> List[Dict]:
        """List all templates"""
        return list(self.templates.values())


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    marketplace = MarketplaceService()

    # Register developer
    dev = marketplace.register_developer("dev1", "john_dev", "john@example.com")
    print(f"Developer registered: {dev.username}")

    # Publish plugin
    listing = marketplace.publish_plugin(
        "Format Helper",
        "dev1",
        "Automatic code formatting",
        PluginCategory.TOOL,
        "https://github.com/john/format-helper",
    )
    print(f"Plugin submitted: {listing.name}")

    # Approve plugin
    marketplace.approve_plugin(listing.id)
    print(f"Plugin approved: {listing.id}")

    # Release version
    release = marketplace.release_version(
        listing.id,
        "1.0.0",
        "Initial release",
        "https://marketplace.com/format-helper-1.0.0.zip",
    )
    print(f"Released: {release.version}")

    # Submit review
    review = marketplace.submit_review(
        listing.id,
        "user2",
        5,
        "Excellent tool!",
        "Works perfectly for formatting code",
    )
    print(f"Review submitted: {review.rating} stars")

    # Search marketplace
    results = marketplace.search_plugins("format", PluginCategory.TOOL)
    print(f"Found {len(results)} plugins")
