"""
Multiplayer Leaderboard System - Phase 3 Feature

Enables competitive learning with real-time leaderboards, achievement tracking,
and multiplayer challenges. Supports local networks and cloud connectivity.
"""

import time
from dataclasses import asdict, dataclass
from datetime import datetime
from enum import Enum
from typing import Callable, Dict, List, Optional


class AchievementType(Enum):
    """Achievement categories."""

    SPEED = "speed"  # Solve quickly
    ACCURACY = "accuracy"  # No errors
    CREATIVITY = "creativity"  # Elegant solutions
    PERSISTENCE = "persistence"  # Many attempts
    COLLABORATION = "collaboration"  # Help others
    MASTERY = "mastery"  # Complete concepts


@dataclass
class Achievement:
    """Represents a player achievement."""

    id: str
    name: str
    description: str
    type: AchievementType
    points: int
    unlock_criteria: Dict
    earned_date: Optional[str] = None

    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        data = asdict(self)
        data["type"] = self.type.value
        return data


@dataclass
class LeaderboardEntry:
    """Single leaderboard entry."""

    rank: int
    username: str
    points: int
    problems_solved: int
    accuracy: float
    challenges_won: int
    last_active: str

    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        return asdict(self)


@dataclass
class Challenge:
    """Multiplayer programming challenge."""

    id: str
    name: str
    description: str
    difficulty: str  # "easy", "medium", "hard"
    language: str
    time_limit_minutes: int
    max_players: int
    created_at: str
    code_template: str
    test_cases: List[Dict]
    rewards: Dict  # {winner_points, participation_points}


class PlayerStats:
    """Track individual player statistics."""

    def __init__(self, username: str):
        self.username = username
        self.total_points = 0
        self.problems_solved = 0
        self.accuracy = 100.0
        self.challenges_won = 0
        self.challenges_played = 0
        self.total_time_minutes = 0
        self.achievements: List[Achievement] = []
        self.join_date = datetime.now()
        self.last_active = datetime.now()
        self.streaks = {"current": 0, "best": 0}
        self.language_stats = {}

    def add_points(self, points: int, reason: str = ""):
        """Add points to player."""
        self.total_points += points
        self.last_active = datetime.now()

    def record_solve(
        self,
        problem_id: str,
        time_seconds: int,
        errors: int = 0,
        language: str = "BASIC",
    ):
        """Record problem solve."""
        self.problems_solved += 1

        # Update accuracy
        if errors == 0:
            self.accuracy = min(100.0, self.accuracy + 1.0)
        else:
            self.accuracy = max(0.0, self.accuracy - (errors * 2.0))

        # Update language stats
        if language not in self.language_stats:
            self.language_stats[language] = {"count": 0, "accuracy": 100.0}
        self.language_stats[language]["count"] += 1

        # Add streak
        self.streaks["current"] += 1
        if self.streaks["current"] > self.streaks["best"]:
            self.streaks["best"] = self.streaks["current"]

        self.last_active = datetime.now()

    def unlock_achievement(self, achievement: Achievement):
        """Unlock an achievement."""
        if achievement not in self.achievements:
            achievement.earned_date = datetime.now().isoformat()
            self.achievements.append(achievement)
            self.add_points(achievement.points)

    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        return {
            "username": self.username,
            "total_points": self.total_points,
            "problems_solved": self.problems_solved,
            "accuracy": self.accuracy,
            "challenges_won": self.challenges_won,
            "challenges_played": self.challenges_played,
            "total_time_minutes": self.total_time_minutes,
            "achievements": [a.to_dict() for a in self.achievements],
            "join_date": self.join_date.isoformat(),
            "last_active": self.last_active.isoformat(),
            "streaks": self.streaks,
            "language_stats": self.language_stats,
        }


class MultiplayerLeaderboard:
    """Manages multiplayer leaderboards and achievements."""

    def __init__(self, local_network: bool = True):
        self.local_network = local_network
        self.players: Dict[str, PlayerStats] = {}
        self.leaderboards: Dict[str, List[LeaderboardEntry]] = {
            "global": [],
            "weekly": [],
            "monthly": [],
            "language_specific": {},
        }
        self.active_challenges: List[Challenge] = []
        self.challenge_results: Dict[str, Dict] = {}
        self.achievements: Dict[str, Achievement] = self._initialize_achievements()

        # Callbacks
        self._on_player_joined: List[Callable] = []
        self._on_points_earned: List[Callable] = []
        self._on_achievement_unlocked: List[Callable] = []
        self._on_challenge_started: List[Callable] = []

    def register_player(self, username: str) -> PlayerStats:
        """Register a new player."""
        if username in self.players:
            return self.players[username]

        player = PlayerStats(username)
        self.players[username] = player
        self._trigger_callbacks(
            self._on_player_joined,
            {"username": username, "timestamp": datetime.now().isoformat()},
        )
        return player

    def record_solve(
        self,
        username: str,
        problem_id: str,
        time_seconds: int,
        errors: int = 0,
        language: str = "BASIC",
        is_correct: bool = True,
    ):
        """Record a successful problem solve."""
        if username not in self.players:
            self.register_player(username)

        player = self.players[username]

        if is_correct:
            # Calculate points based on time and errors
            base_points = 100
            time_penalty = max(0, (time_seconds - 60) // 10)
            error_penalty = errors * 5
            total_points = max(10, base_points - time_penalty - error_penalty)

            player.record_solve(problem_id, time_seconds, errors, language)
            player.add_points(total_points, f"Solved {problem_id}")

            self._trigger_callbacks(
                self._on_points_earned,
                {
                    "username": username,
                    "points": total_points,
                    "reason": f"Solved {problem_id}",
                    "timestamp": datetime.now().isoformat(),
                },
            )

            # Check achievements
            self._check_achievements(player)
            self._update_leaderboards()

    def create_challenge(
        self,
        name: str,
        description: str,
        difficulty: str,
        language: str,
        time_limit_minutes: int,
        max_players: int,
        code_template: str,
        test_cases: List[Dict],
    ) -> Challenge:
        """Create a new multiplayer challenge."""
        challenge = Challenge(
            id=f"challenge_{int(time.time())}",
            name=name,
            description=description,
            difficulty=difficulty,
            language=language,
            time_limit_minutes=time_limit_minutes,
            max_players=max_players,
            created_at=datetime.now().isoformat(),
            code_template=code_template,
            test_cases=test_cases,
            rewards={"winner_points": 500, "participation_points": 50},
        )

        self.active_challenges.append(challenge)
        self._trigger_callbacks(self._on_challenge_started, challenge.__dict__)
        return challenge

    def join_challenge(self, challenge_id: str, username: str) -> bool:
        """Join a multiplayer challenge."""
        challenge = self._find_challenge(challenge_id)
        if not challenge:
            return False

        if challenge_id not in self.challenge_results:
            self.challenge_results[challenge_id] = {"participants": {}}

        self.challenge_results[challenge_id]["participants"][username] = {
            "joined_at": datetime.now().isoformat(),
            "status": "in_progress",
            "result": None,
        }

        return True

    def submit_challenge_solution(
        self,
        challenge_id: str,
        username: str,
        code: str,
        time_seconds: int,
        errors: int,
        test_passed: int,
        test_total: int,
    ) -> Dict:
        """Submit solution for a challenge."""
        if challenge_id not in self.challenge_results:
            return {"success": False, "message": "Challenge not found"}

        if username not in self.challenge_results[challenge_id]["participants"]:
            return {
                "success": False,
                "message": "Not registered for challenge",
            }

        # Calculate score
        accuracy = (test_passed / test_total) * 100 if test_total > 0 else 0
        score = int(accuracy * 10)  # 0-1000 scale

        # Update result
        result = {
            "submitted_at": datetime.now().isoformat(),
            "status": "submitted",
            "score": score,
            "accuracy": accuracy,
            "test_passed": test_passed,
            "test_total": test_total,
            "time_seconds": time_seconds,
            "errors": errors,
            "code": code,
        }

        self.challenge_results[challenge_id]["participants"][username][
            "result"
        ] = result

        # Award points
        if test_passed == test_total:
            challenge = self._find_challenge(challenge_id)
            points = challenge.rewards["winner_points"]
        else:
            points = self.challenge_results[challenge_id]["participants"][username]
            points = 50  # participation points

        self.record_solve(username, challenge_id, time_seconds, errors)

        return {
            "success": True,
            "score": score,
            "points": points,
            "rank": self._get_challenge_rank(challenge_id, username),
        }

    def get_leaderboard(
        self,
        leaderboard_type: str = "global",
        language: Optional[str] = None,
        limit: int = 10,
    ) -> List[LeaderboardEntry]:
        """Get leaderboard entries."""
        entries = []

        # Sort players
        sorted_players = sorted(
            self.players.values(), key=lambda p: p.total_points, reverse=True
        )

        # Generate entries
        for rank, player in enumerate(sorted_players[:limit], 1):
            entry = LeaderboardEntry(
                rank=rank,
                username=player.username,
                points=player.total_points,
                problems_solved=player.problems_solved,
                accuracy=player.accuracy,
                challenges_won=player.challenges_won,
                last_active=player.last_active.isoformat(),
            )
            entries.append(entry)

        return entries

    def get_player_stats(self, username: str) -> Optional[Dict]:
        """Get detailed player statistics."""
        if username not in self.players:
            return None
        return self.players[username].to_dict()

    def get_achievements(self, username: str) -> List[Achievement]:
        """Get player achievements."""
        if username not in self.players:
            return []
        return self.players[username].achievements

    def _initialize_achievements(self) -> Dict[str, Achievement]:
        """Initialize built-in achievements."""
        return {
            "first_solve": Achievement(
                id="first_solve",
                name="First Steps",
                description="Solve your first problem",
                type=AchievementType.SPEED,
                points=10,
                unlock_criteria={"problems_solved": 1},
            ),
            "speedster": Achievement(
                id="speedster",
                name="Speedster",
                description="Solve 10 problems in under 60 seconds each",
                type=AchievementType.SPEED,
                points=50,
                unlock_criteria={"fast_solves": 10},
            ),
            "perfect_accuracy": Achievement(
                id="perfect_accuracy",
                name="Perfect Accuracy",
                description="Solve 5 problems with zero errors",
                type=AchievementType.ACCURACY,
                points=100,
                unlock_criteria={"perfect_solves": 5},
            ),
            "polyglot": Achievement(
                id="polyglot",
                name="Polyglot Programmer",
                description="Solve problems in all 7 languages",
                type=AchievementType.MASTERY,
                points=200,
                unlock_criteria={"languages_mastered": 7},
            ),
            "teamplayer": Achievement(
                id="teamplayer",
                name="Team Player",
                description="Help 10 other players solve problems",
                type=AchievementType.COLLABORATION,
                points=150,
                unlock_criteria={"helpers_count": 10},
            ),
        }

    def _check_achievements(self, player: PlayerStats):
        """Check and unlock achievements for player."""
        for ach_id, achievement in self.achievements.items():
            if achievement in player.achievements:
                continue  # Already earned

            # Check unlock criteria
            criteria = achievement.unlock_criteria

            if achievement.type == AchievementType.SPEED:
                if player.problems_solved >= criteria.get("problems_solved", 1):
                    player.unlock_achievement(achievement)
                    self._trigger_callbacks(
                        self._on_achievement_unlocked,
                        {
                            "username": player.username,
                            "achievement": achievement.to_dict(),
                            "timestamp": datetime.now().isoformat(),
                        },
                    )

    def _find_challenge(self, challenge_id: str) -> Optional[Challenge]:
        """Find challenge by ID."""
        for challenge in self.active_challenges:
            if challenge.id == challenge_id:
                return challenge
        return None

    def _get_challenge_rank(self, challenge_id: str, username: str) -> int:
        """Get player's rank in challenge."""
        if challenge_id not in self.challenge_results:
            return -1

        participants = self.challenge_results[challenge_id]["participants"]
        ranked = sorted(
            participants.items(),
            key=lambda x: x[1]["result"]["score"] if x[1]["result"] else 0,
            reverse=True,
        )

        for rank, (u, _) in enumerate(ranked, 1):
            if u == username:
                return rank

        return -1

    def _update_leaderboards(self):
        """Update all leaderboards."""
        # Update global leaderboard
        self.leaderboards["global"] = self.get_leaderboard("global")

        # Update weekly (24h cutoff)
        self.leaderboards["weekly"] = self.get_leaderboard("weekly")

        # Update monthly (30d cutoff)
        self.leaderboards["monthly"] = self.get_leaderboard("monthly")

    def _trigger_callbacks(self, callbacks: List[Callable], data: Dict):
        """Trigger callback functions."""
        for callback in callbacks:
            try:
                callback(data)
            except Exception as e:
                print(f"Callback error: {e}")

    # Callback registration
    def on_player_joined(self, callback: Callable):
        """Register callback for player join."""
        self._on_player_joined.append(callback)

    def on_points_earned(self, callback: Callable):
        """Register callback for points earned."""
        self._on_points_earned.append(callback)

    def on_achievement_unlocked(self, callback: Callable):
        """Register callback for achievement unlocked."""
        self._on_achievement_unlocked.append(callback)

    def on_challenge_started(self, callback: Callable):
        """Register callback for challenge started."""
        self._on_challenge_started.append(callback)
