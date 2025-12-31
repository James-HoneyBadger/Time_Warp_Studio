"""
Community Marketplace - Phase 3 Feature

Enables sharing and discovery of:
- Custom templates
- Reusable code snippets
- Game assets
- Tutorials and lessons
- Educational projects

With ratings, reviews, and revenue sharing for educators.
"""

import json
import hashlib
from typing import Dict, List, Optional, Tuple
from datetime import datetime
from enum import Enum
from dataclasses import dataclass, asdict


class ItemType(Enum):
    """Marketplace item types."""
    TEMPLATE = "template"
    SNIPPET = "snippet"
    ASSET = "asset"
    TUTORIAL = "tutorial"
    PROJECT = "project"
    LESSON = "lesson"


class ItemStatus(Enum):
    """Item publication status."""
    DRAFT = "draft"
    REVIEW = "review"
    PUBLISHED = "published"
    ARCHIVED = "archived"
    SUSPENDED = "suspended"


@dataclass
class ItemRating:
    """Rating for marketplace item."""
    user_id: str
    username: str
    rating: int  # 1-5 stars
    review: str
    helpful_count: int
    date: str


@dataclass
class MarketplaceItem:
    """Item in marketplace."""
    id: str
    title: str
    description: str
    item_type: ItemType
    author_id: str
    author_name: str
    author_avatar_url: str
    content: str
    language: str
    category: str
    tags: List[str]
    created_at: str
    updated_at: str
    version: str
    license: str  # MIT, CC-BY, GPL, etc.
    status: ItemStatus
    download_count: int
    rating_avg: float
    rating_count: int
    price: float  # 0 for free
    preview_url: Optional[str] = None
    dependencies: List[str] = None
    
    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        data = asdict(self)
        data['item_type'] = self.item_type.value
        data['status'] = self.status.value
        if self.dependencies is None:
            data['dependencies'] = []
        return data


@dataclass
class ItemStats:
    """Statistics for marketplace item."""
    item_id: str
    downloads: int
    views: int
    stars_earned: float
    revenue: float
    favorites: int
    shares: int
    reports: int
    updated_at: str


class CommunityMarketplace:
    """Main marketplace management system."""
    
    def __init__(self):
        self.items: Dict[str, MarketplaceItem] = {}
        self.ratings: Dict[str, List[ItemRating]] = {}
        self.stats: Dict[str, ItemStats] = {}
        self.categories = [
            "Games", "Graphics", "Math", "Education", "Utilities",
            "Advanced", "Beginner", "Intermediate", "Expert"
        ]
        self.user_favorites: Dict[str, List[str]] = {}
        self.featured_items: List[str] = []
        self.trending_items: List[str] = []
    
    def publish_item(self, title: str, description: str, item_type: ItemType,
                    author_id: str, author_name: str, author_avatar: str,
                    content: str, language: str, category: str,
                    tags: List[str], license: str,
                    price: float = 0.0,
                    preview_url: Optional[str] = None) -> MarketplaceItem:
        """Publish a new item to marketplace."""
        
        item_id = self._generate_id(title, author_id)
        
        item = MarketplaceItem(
            id=item_id,
            title=title,
            description=description,
            item_type=item_type,
            author_id=author_id,
            author_name=author_name,
            author_avatar_url=author_avatar,
            content=content,
            language=language,
            category=category,
            tags=tags,
            created_at=datetime.now().isoformat(),
            updated_at=datetime.now().isoformat(),
            version="1.0.0",
            license=license,
            status=ItemStatus.REVIEW,
            download_count=0,
            rating_avg=0.0,
            rating_count=0,
            price=price,
            preview_url=preview_url,
            dependencies=[]
        )
        
        self.items[item_id] = item
        self.stats[item_id] = ItemStats(
            item_id=item_id,
            downloads=0,
            views=0,
            stars_earned=0.0,
            revenue=0.0,
            favorites=0,
            shares=0,
            reports=0,
            updated_at=datetime.now().isoformat()
        )
        
        return item
    
    def approve_item(self, item_id: str) -> bool:
        """Approve item for publication."""
        if item_id not in self.items:
            return False
        
        self.items[item_id].status = ItemStatus.PUBLISHED
        self.items[item_id].updated_at = datetime.now().isoformat()
        return True
    
    def reject_item(self, item_id: str, reason: str) -> bool:
        """Reject item publication."""
        if item_id not in self.items:
            return False
        
        self.items[item_id].status = ItemStatus.DRAFT
        return True
    
    def search_items(self, query: str = "", item_type: Optional[ItemType] = None,
                    category: Optional[str] = None, language: Optional[str] = None,
                    sort_by: str = "rating", limit: int = 20) -> List[MarketplaceItem]:
        """Search marketplace items."""
        
        results = [item for item in self.items.values()
                  if item.status == ItemStatus.PUBLISHED]
        
        # Filter by query
        if query:
            query_lower = query.lower()
            results = [item for item in results
                      if query_lower in item.title.lower() or
                         query_lower in item.description.lower() or
                         any(query_lower in tag for tag in item.tags)]
        
        # Filter by type
        if item_type:
            results = [item for item in results if item.item_type == item_type]
        
        # Filter by category
        if category:
            results = [item for item in results if item.category == category]
        
        # Filter by language
        if language:
            results = [item for item in results if item.language == language]
        
        # Sort
        if sort_by == "rating":
            results.sort(key=lambda x: x.rating_avg, reverse=True)
        elif sort_by == "downloads":
            results.sort(key=lambda x: x.download_count, reverse=True)
        elif sort_by == "recent":
            results.sort(key=lambda x: x.updated_at, reverse=True)
        elif sort_by == "trending":
            results.sort(key=lambda x: self.stats.get(x.id, ItemStats(
                x.id, 0, 0, 0, 0, 0, 0, 0, "")).views, reverse=True)
        
        return results[:limit]
    
    def get_item(self, item_id: str) -> Optional[MarketplaceItem]:
        """Get item by ID."""
        item = self.items.get(item_id)
        if item:
            # Increment view count
            if item_id in self.stats:
                self.stats[item_id].views += 1
                self.stats[item_id].updated_at = datetime.now().isoformat()
        return item
    
    def download_item(self, item_id: str, user_id: str) -> Optional[str]:
        """Download item content."""
        if item_id not in self.items:
            return None
        
        item = self.items[item_id]
        if item.status != ItemStatus.PUBLISHED:
            return None
        
        # Increment download count
        item.download_count += 1
        if item_id in self.stats:
            self.stats[item_id].downloads += 1
        
        return item.content
    
    def rate_item(self, item_id: str, user_id: str, username: str,
                 rating: int, review: str = "") -> bool:
        """Rate and review item."""
        if item_id not in self.items:
            return False
        
        if not (1 <= rating <= 5):
            return False
        
        # Create rating
        item_rating = ItemRating(
            user_id=user_id,
            username=username,
            rating=rating,
            review=review,
            helpful_count=0,
            date=datetime.now().isoformat()
        )
        
        if item_id not in self.ratings:
            self.ratings[item_id] = []
        
        self.ratings[item_id].append(item_rating)
        
        # Update item rating
        item = self.items[item_id]
        total_rating = sum(r.rating for r in self.ratings[item_id])
        item.rating_avg = total_rating / len(self.ratings[item_id])
        item.rating_count = len(self.ratings[item_id])
        
        return True
    
    def add_to_favorites(self, item_id: str, user_id: str) -> bool:
        """Add item to user favorites."""
        if item_id not in self.items:
            return False
        
        if user_id not in self.user_favorites:
            self.user_favorites[user_id] = []
        
        if item_id not in self.user_favorites[user_id]:
            self.user_favorites[user_id].append(item_id)
            
            if item_id in self.stats:
                self.stats[item_id].favorites += 1
        
        return True
    
    def remove_from_favorites(self, item_id: str, user_id: str) -> bool:
        """Remove item from user favorites."""
        if user_id not in self.user_favorites:
            return False
        
        if item_id in self.user_favorites[user_id]:
            self.user_favorites[user_id].remove(item_id)
            
            if item_id in self.stats:
                self.stats[item_id].favorites = max(0, self.stats[item_id].favorites - 1)
        
        return True
    
    def get_user_items(self, author_id: str) -> List[MarketplaceItem]:
        """Get all items by author."""
        return [item for item in self.items.values()
               if item.author_id == author_id]
    
    def get_featured_items(self, limit: int = 10) -> List[MarketplaceItem]:
        """Get featured items."""
        featured = [self.items[iid] for iid in self.featured_items
                   if iid in self.items]
        return featured[:limit]
    
    def get_trending_items(self, limit: int = 10) -> List[MarketplaceItem]:
        """Get trending items (by downloads this week)."""
        sorted_items = sorted(
            [item for item in self.items.values()
            if item.status == ItemStatus.PUBLISHED],
            key=lambda x: self.stats.get(x.id, ItemStats(x.id, 0, 0, 0, 0, 0, 0, 0, "")).downloads,
            reverse=True
        )
        return sorted_items[:limit]
    
    def get_item_reviews(self, item_id: str) -> List[ItemRating]:
        """Get all reviews for item."""
        return self.ratings.get(item_id, [])
    
    def report_item(self, item_id: str, reason: str, user_id: str) -> bool:
        """Report inappropriate item."""
        if item_id not in self.items:
            return False
        
        if item_id in self.stats:
            self.stats[item_id].reports += 1
            
            # Auto-suspend if too many reports
            if self.stats[item_id].reports >= 5:
                self.items[item_id].status = ItemStatus.SUSPENDED
        
        return True
    
    def update_item(self, item_id: str, **kwargs) -> bool:
        """Update item details."""
        if item_id not in self.items:
            return False
        
        item = self.items[item_id]
        
        allowed_fields = {
            "title", "description", "content", "tags", "category",
            "preview_url", "dependencies", "license"
        }
        
        for key, value in kwargs.items():
            if key in allowed_fields and hasattr(item, key):
                setattr(item, key, value)
        
        item.updated_at = datetime.now().isoformat()
        return True
    
    def get_statistics(self, item_id: str) -> Optional[ItemStats]:
        """Get item statistics."""
        return self.stats.get(item_id)
    
    def get_author_earnings(self, author_id: str) -> float:
        """Calculate author earnings."""
        total = 0.0
        for item in self.items.values():
            if item.author_id == author_id and item.price > 0:
                downloads = self.stats.get(item.id, ItemStats(item.id, 0, 0, 0, 0, 0, 0, 0, "")).downloads
                # Standard 70/30 split (author gets 70%)
                total += (item.price * downloads * 0.70)
        return total
    
    def _generate_id(self, title: str, author_id: str) -> str:
        """Generate unique item ID."""
        base = f"{title}_{author_id}_{datetime.now().timestamp()}"
        return hashlib.md5(base.encode()).hexdigest()[:12]
    
    def get_categories(self) -> List[str]:
        """Get all categories."""
        return self.categories
    
    def get_status(self) -> Dict:
        """Get marketplace statistics."""
        published_items = [item for item in self.items.values()
                          if item.status == ItemStatus.PUBLISHED]
        
        return {
            "total_items": len(self.items),
            "published_items": len(published_items),
            "total_downloads": sum(self.stats.get(iid, ItemStats(iid, 0, 0, 0, 0, 0, 0, 0, "")).downloads
                                  for iid in self.items),
            "average_rating": (sum(item.rating_avg * item.rating_count for item in published_items) /
                             sum(item.rating_count for item in published_items)
                             if published_items else 0.0),
            "total_authors": len(set(item.author_id for item in published_items)),
            "categories": len(self.categories),
            "updated_at": datetime.now().isoformat()
        }
