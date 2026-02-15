# pylint: disable=redefined-outer-name
"""
Time Warp Studio - Community & Social Features

Provides:
- User profiles and social graphs
- Code snippets and sharing
- Challenges and competitions
- Leaderboards and achievements
- Community forums
"""

import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Dict, List, Optional, Set


def utc_now() -> datetime:
    return datetime.now(timezone.utc)


# ===== ENUMS =====


class ChallengeLevel(Enum):
    """Challenge difficulty levels"""

    BEGINNER = "beginner"
    INTERMEDIATE = "intermediate"
    ADVANCED = "advanced"
    EXPERT = "expert"


class AchievementType(Enum):
    """Achievement types"""

    FIRST_CODE = "first_code"
    PROLIFIC_AUTHOR = "prolific_author"
    HELPFUL_COMMUNITY = "helpful_community"
    CHALLENGE_MASTER = "challenge_master"
    LANGUAGE_EXPERT = "language_expert"
    COLLABORATOR = "collaborator"
    SPEEDSTER = "speedster"
    DEBUGGER = "debugger"


# ===== DATA CLASSES =====


@dataclass
class UserProfile:
    """Extended user profile"""

    user_id: str
    username: str
    email: str
    avatar_url: Optional[str] = None
    bio: Optional[str] = None
    location: Optional[str] = None
    website: Optional[str] = None

    # Stats
    followers: int = 0
    following: int = 0
    total_snippets: int = 0
    total_contributions: int = 0
    reputation_score: int = 0

    # Preferences
    language_preferences: List[str] = field(default_factory=list)
    theme_preference: str = "dark"
    notifications_enabled: bool = True

    # Dates
    created_at: datetime = field(default_factory=utc_now)
    last_active: datetime = field(default_factory=utc_now)


@dataclass
class CodeSnippet:
    """Shared code snippet"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    user_id: str = ""
    title: str = ""
    description: str = ""
    code: str = ""
    language: str = ""

    tags: List[str] = field(default_factory=list)

    # Stats
    views: int = 0
    likes: int = 0
    forks: int = 0
    comments: int = 0

    # Metadata
    created_at: datetime = field(default_factory=utc_now)
    updated_at: datetime = field(default_factory=utc_now)
    is_public: bool = True
    is_pinned: bool = False


@dataclass
class Challenge:
    """Coding challenge"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    title: str = ""
    description: str = ""
    problem_statement: str = ""

    language: str = "basic"
    level: ChallengeLevel = ChallengeLevel.BEGINNER

    # Test cases
    test_cases: List[Dict] = field(default_factory=list)

    # Metadata
    author_id: str = ""
    created_at: datetime = field(default_factory=utc_now)
    deadline: Optional[datetime] = None

    # Participation
    participants: int = 0
    solutions_submitted: int = 0
    avg_completion_time_minutes: int = 0

    # Rewards
    xp_reward: int = 100
    reputation_reward: int = 50


@dataclass
class ChallengeSubmission:
    """User's challenge submission"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    challenge_id: str = ""
    user_id: str = ""
    code: str = ""

    # Results
    passed_tests: int = 0
    total_tests: int = 0
    execution_time_ms: float = 0.0

    # Metadata
    submitted_at: datetime = field(default_factory=utc_now)
    completed: bool = False
    completion_time_minutes: int = 0

    # Feedback
    feedback: str = ""
    test_results: List[Dict] = field(default_factory=list)


@dataclass
class Achievement:
    """User achievement"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    user_id: str = ""
    achievement_type: AchievementType = AchievementType.FIRST_CODE

    title: str = ""
    description: str = ""
    icon_url: Optional[str] = None

    progress: int = 0  # 0-100
    unlocked: bool = False
    unlocked_at: Optional[datetime] = None

    # Rewards
    xp_reward: int = 0
    reputation_reward: int = 0


@dataclass
class ForumPost:
    """Community forum post"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    author_id: str = ""
    title: str = ""
    content: str = ""
    category: str = ""  # help, discussion, showcase, bug

    # Stats
    views: int = 0
    replies: int = 0
    helpful_votes: int = 0

    # Metadata
    created_at: datetime = field(default_factory=utc_now)
    updated_at: datetime = field(default_factory=utc_now)
    pinned: bool = False
    closed: bool = False

    # Tags
    tags: List[str] = field(default_factory=list)


@dataclass
class ForumReply:
    """Forum post reply"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    post_id: str = ""
    author_id: str = ""
    content: str = ""

    # Stats
    helpful_votes: int = 0
    is_accepted_answer: bool = False

    # Metadata
    created_at: datetime = field(default_factory=utc_now)
    updated_at: datetime = field(default_factory=utc_now)


# ===== SERVICES =====


class ProfileService:
    """User profile management"""

    def __init__(self):
        self.profiles: Dict[str, UserProfile] = {}
        self.follows: Dict[str, Set[str]] = {}  # user -> following

    def create_profile(self, user_id: str, username: str, email: str) -> UserProfile:
        """Create new user profile"""
        profile = UserProfile(user_id=user_id, username=username, email=email)
        self.profiles[user_id] = profile
        return profile

    def get_profile(self, user_id: str) -> Optional[UserProfile]:
        """Get user profile"""
        return self.profiles.get(user_id)

    def update_profile(self, user_id: str, **kwargs) -> bool:
        """Update user profile"""
        profile = self.profiles.get(user_id)
        if not profile:
            return False

        for key, value in kwargs.items():
            if hasattr(profile, key):
                setattr(profile, key, value)

        profile.last_active = utc_now()
        return True

    def follow(self, user_id: str, target_user_id: str) -> bool:
        """Follow another user"""
        if user_id not in self.follows:
            self.follows[user_id] = set()

        if target_user_id in self.follows[user_id]:
            return False  # Already following

        self.follows[user_id].add(target_user_id)

        # Update counts
        user = self.profiles.get(user_id)
        target = self.profiles.get(target_user_id)

        if user:
            user.following += 1
        if target:
            target.followers += 1

        return True

    def unfollow(self, user_id: str, target_user_id: str) -> bool:
        """Unfollow user"""
        if user_id not in self.follows:
            return False

        if target_user_id not in self.follows[user_id]:
            return False

        self.follows[user_id].remove(target_user_id)

        # Update counts
        user = self.profiles.get(user_id)
        target = self.profiles.get(target_user_id)

        if user:
            user.following = max(0, user.following - 1)
        if target:
            target.followers = max(0, target.followers - 1)

        return True

    def get_followers(self, user_id: str) -> List[UserProfile]:
        """Get user's followers"""
        followers = []
        for follower_id, following in self.follows.items():
            if user_id in following:
                profile = self.profiles.get(follower_id)
                if profile:
                    followers.append(profile)
        return followers

    def get_leaderboard(self, limit: int = 100) -> List[UserProfile]:
        """Get reputation leaderboard"""
        return sorted(
            self.profiles.values(),
            key=lambda p: p.reputation_score,
            reverse=True,
        )[:limit]


class ChallengeService:
    """Challenge management"""

    def __init__(self):
        self.challenges: Dict[str, Challenge] = {}
        self.submissions: Dict[str, ChallengeSubmission] = {}

    def create_challenge(
        self,
        title: str,
        description: str,
        problem_statement: str,
        language: str,
        level: ChallengeLevel,
        test_cases: List[Dict],
        author_id: str,
    ) -> Challenge:
        """Create new challenge"""
        challenge = Challenge(
            title=title,
            description=description,
            problem_statement=problem_statement,
            language=language,
            level=level,
            test_cases=test_cases,
            author_id=author_id,
        )
        self.challenges[challenge.id] = challenge
        return challenge

    def submit_solution(
        self, challenge_id: str, user_id: str, code: str
    ) -> ChallengeSubmission:
        """Submit challenge solution"""
        submission = ChallengeSubmission(
            challenge_id=challenge_id, user_id=user_id, code=code
        )

        # Evaluate solution (placeholder)
        passed = len([t for t in self.challenges[challenge_id].test_cases])
        submission.passed_tests = passed
        submission.total_tests = passed
        submission.completed = passed == len(self.challenges[challenge_id].test_cases)

        self.submissions[submission.id] = submission
        return submission

    def get_challenge_leaderboard(
        self, challenge_id: str, limit: int = 50
    ) -> List[tuple[str, int, float]]:
        """Get challenge leaderboard"""
        challenge_submissions = [
            s
            for s in self.submissions.values()
            if s.challenge_id == challenge_id and s.completed
        ]

        # Sort by completion time
        sorted_submissions = sorted(
            challenge_submissions, key=lambda s: s.completion_time_minutes
        )

        return [
            (s.user_id, s.passed_tests, s.execution_time_ms)
            for s in sorted_submissions[:limit]
        ]


class CommunityService:
    """Community management"""

    def __init__(self):
        self.posts: Dict[str, ForumPost] = {}
        self.replies: Dict[str, ForumReply] = {}
        self.snippets: Dict[str, CodeSnippet] = {}
        self.achievements: Dict[str, List[Achievement]] = {}

    def create_forum_post(
        self,
        author_id: str,
        title: str,
        content: str,
        category: str,
        tags: List[str],
    ) -> ForumPost:
        """Create forum post"""
        post = ForumPost(
            author_id=author_id,
            title=title,
            content=content,
            category=category,
            tags=tags,
        )
        self.posts[post.id] = post
        return post

    def reply_to_post(self, post_id: str, author_id: str, content: str) -> ForumReply:
        """Reply to forum post"""
        reply = ForumReply(post_id=post_id, author_id=author_id, content=content)
        self.replies[reply.id] = reply

        # Update post reply count
        if post_id in self.posts:
            self.posts[post_id].replies += 1

        return reply

    def share_snippet(
        self,
        user_id: str,
        title: str,
        description: str,
        code: str,
        language: str,
        tags: List[str],
        is_public: bool = True,
    ) -> CodeSnippet:
        """Share code snippet"""
        snippet = CodeSnippet(
            user_id=user_id,
            title=title,
            description=description,
            code=code,
            language=language,
            tags=tags,
            is_public=is_public,
        )
        self.snippets[snippet.id] = snippet
        return snippet

    def unlock_achievement(
        self,
        user_id: str,
        achievement_type: AchievementType,
        title: str,
        description: str,
        xp_reward: int = 0,
        reputation_reward: int = 0,
    ) -> Achievement:
        """Unlock user achievement"""
        achievement = Achievement(
            user_id=user_id,
            achievement_type=achievement_type,
            title=title,
            description=description,
            xp_reward=xp_reward,
            reputation_reward=reputation_reward,
            unlocked=True,
            unlocked_at=utc_now(),
        )

        if user_id not in self.achievements:
            self.achievements[user_id] = []

        self.achievements[user_id].append(achievement)
        return achievement

    def get_user_achievements(self, user_id: str) -> List[Achievement]:
        """Get user's achievements"""
        return self.achievements.get(user_id, [])

    def search_snippets(
        self,
        query: str = "",
        language: Optional[str] = None,
        tags: Optional[List[str]] = None,
        limit: int = 20,
    ) -> List[CodeSnippet]:
        """Search code snippets"""
        results = list(self.snippets.values())

        if query:
            results = [
                s
                for s in results
                if query.lower() in s.title.lower()
                or query.lower() in s.description.lower()
            ]

        if language:
            results = [s for s in results if s.language == language]

        if tags:
            results = [s for s in results if any(tag in s.tags for tag in tags)]

        # Sort by popularity
        results.sort(key=lambda s: s.views + s.likes * 5, reverse=True)

        return results[:limit]


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    # Create services
    profile_service = ProfileService()
    challenge_service = ChallengeService()
    community_service = CommunityService()

    # Create user profile
    profile = profile_service.create_profile("user_1", "john_doe", "john@example.com")
    print(f"Created profile: {profile.username}")

    # Create challenge
    challenge = challenge_service.create_challenge(
        "Hello World",
        "Write a program that prints Hello, World!",
        "Your task is to write a program...",
        "basic",
        ChallengeLevel.BEGINNER,
        [{"input": "", "expected_output": "Hello, World!"}],
        "admin",
    )
    print(f"Created challenge: {challenge.title}")

    # Submit solution
    submission = challenge_service.submit_solution(
        challenge.id, "user_1", 'PRINT "Hello, World!"'
    )
    print(f"Submitted solution: {submission.completed}")

    # Share snippet
    snippet = community_service.share_snippet(
        "user_1",
        "Hello World",
        "Simple hello world program",
        'PRINT "Hello, World!"',
        "basic",
        ["beginner", "hello"],
    )
    print(f"Shared snippet: {snippet.title}")
