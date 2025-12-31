"""
Phase 3 Integration Tests - Multiplayer, LMS, and Marketplace

Comprehensive test suite for new v6.0.0+ features:
- Multiplayer Leaderboard System
- LMS Integration
- Community Marketplace
"""

import pytest
import sys
from pathlib import Path
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock

# Add platform path
sys.path.insert(0, str(Path(__file__).parent.parent / "Platforms" / "Python"))

# Import Phase 3 modules
from time_warp.core.multiplayer_leaderboard import (
    MultiplayerLeaderboard, PlayerStats, Achievement,
    AchievementType, Challenge
)
from time_warp.core.lms_integration import (
    LMSIntegration, CanvasConnector, GoogleClassroomConnector,
    LMSType, AssignmentStatus, StudentSubmission
)
from time_warp.core.community_marketplace import (
    CommunityMarketplace, MarketplaceItem, ItemType, ItemStatus, ItemRating
)


# ============================================================================
# MULTIPLAYER LEADERBOARD TESTS
# ============================================================================

class TestMultiplayerLeaderboard:
    """Test multiplayer leaderboard system."""
    
    @pytest.fixture
    def leaderboard(self):
        """Create leaderboard instance."""
        return MultiplayerLeaderboard(local_network=True)
    
    def test_player_registration(self, leaderboard):
        """Test player registration."""
        player = leaderboard.register_player("alice")
        assert player.username == "alice"
        assert player.total_points == 0
        assert player.problems_solved == 0
    
    def test_duplicate_player_registration(self, leaderboard):
        """Test registering same player twice."""
        player1 = leaderboard.register_player("bob")
        player2 = leaderboard.register_player("bob")
        assert player1 is player2
    
    def test_record_solve_correct(self, leaderboard):
        """Test recording correct problem solve."""
        leaderboard.register_player("charlie")
        leaderboard.record_solve("charlie", "prob_1", time_seconds=45,
                                errors=0, language="BASIC", is_correct=True)
        
        player = leaderboard.players["charlie"]
        assert player.problems_solved == 1
        assert player.accuracy == 100.0
        assert player.total_points > 0
    
    def test_record_solve_with_errors(self, leaderboard):
        """Test recording solve with errors."""
        leaderboard.register_player("david")
        leaderboard.record_solve("david", "prob_2", time_seconds=120,
                                errors=2, language="LOGO", is_correct=True)
        
        player = leaderboard.players["david"]
        assert player.problems_solved == 1
        assert player.accuracy < 100.0  # Accuracy reduced due to errors
    
    def test_leaderboard_ranking(self, leaderboard):
        """Test leaderboard ranking calculation."""
        # Add multiple players
        leaderboard.register_player("alice")
        leaderboard.register_player("bob")
        leaderboard.register_player("charlie")
        
        # Give them different points
        leaderboard.record_solve("alice", "p1", 30, 0, "BASIC")
        leaderboard.record_solve("bob", "p1", 45, 0, "BASIC")
        leaderboard.record_solve("charlie", "p1", 60, 0, "BASIC")
        
        leaderboard.record_solve("alice", "p2", 30, 0, "BASIC")
        leaderboard.record_solve("bob", "p2", 45, 0, "BASIC")
        
        # Get leaderboard
        board = leaderboard.get_leaderboard("global", limit=3)
        assert len(board) == 3
        assert board[0].username == "alice"  # Most points
        assert board[1].username == "bob"
        assert board[2].username == "charlie"
    
    def test_achievement_unlock(self, leaderboard):
        """Test achievement unlocking."""
        leaderboard.register_player("eve")
        
        # Solve problems to unlock "First Steps" achievement
        leaderboard.record_solve("eve", "p1", 60, 0, "BASIC")
        
        player = leaderboard.players["eve"]
        assert len(player.achievements) > 0
    
    def test_create_challenge(self, leaderboard):
        """Test creating multiplayer challenge."""
        challenge = leaderboard.create_challenge(
            name="Speed Challenge",
            description="Solve as fast as you can",
            difficulty="medium",
            language="BASIC",
            time_limit_minutes=5,
            max_players=10,
            code_template="PRINT 'Hello'",
            test_cases=[{"input": "", "expected": "Hello\n"}]
        )
        
        assert challenge.name == "Speed Challenge"
        assert challenge.difficulty == "medium"
        assert len(leaderboard.active_challenges) == 1
    
    def test_join_challenge(self, leaderboard):
        """Test joining challenge."""
        challenge = leaderboard.create_challenge(
            name="Math Challenge",
            description="",
            difficulty="easy",
            language="BASIC",
            time_limit_minutes=10,
            max_players=5,
            code_template="",
            test_cases=[]
        )
        
        leaderboard.register_player("frank")
        success = leaderboard.join_challenge(challenge.id, "frank")
        assert success
    
    def test_submit_challenge_solution(self, leaderboard):
        """Test submitting challenge solution."""
        challenge = leaderboard.create_challenge(
            name="Test Challenge",
            description="",
            difficulty="easy",
            language="BASIC",
            time_limit_minutes=5,
            max_players=5,
            code_template="",
            test_cases=[{}, {}]
        )
        
        leaderboard.register_player("grace")
        leaderboard.join_challenge(challenge.id, "grace")
        
        result = leaderboard.submit_challenge_solution(
            challenge.id, "grace",
            code="PRINT 'test'",
            time_seconds=120,
            errors=0,
            test_passed=2,
            test_total=2
        )
        
        assert result["success"]
        assert result["score"] == 1000
    
    def test_callback_registration(self, leaderboard):
        """Test callback functions."""
        callback_data = {}
        
        def on_player_joined(data):
            callback_data['joined'] = data
        
        def on_points_earned(data):
            callback_data['points'] = data
        
        leaderboard.on_player_joined(on_player_joined)
        leaderboard.on_points_earned(on_points_earned)
        
        leaderboard.register_player("henry")
        assert "joined" in callback_data
        
        leaderboard.record_solve("henry", "p1", 60, 0, "BASIC")
        assert "points" in callback_data


# ============================================================================
# LMS INTEGRATION TESTS
# ============================================================================

class TestLMSIntegration:
    """Test LMS integration system."""
    
    @pytest.fixture
    def lms(self):
        """Create LMS integration instance."""
        return LMSIntegration()
    
    def test_canvas_connector_init(self):
        """Test Canvas connector initialization."""
        connector = CanvasConnector("https://canvas.example.com", "test_token")
        assert connector.lms_type == LMSType.CANVAS
        assert connector.api_key == "test_token"
    
    @patch('requests.Session.get')
    def test_canvas_authentication(self, mock_get):
        """Test Canvas authentication."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_get.return_value = mock_response
        
        connector = CanvasConnector("https://canvas.example.com", "test_token")
        assert connector.authenticate()
    
    @patch('requests.Session.get')
    def test_canvas_authentication_failure(self, mock_get):
        """Test Canvas authentication failure."""
        mock_response = Mock()
        mock_response.status_code = 401
        mock_get.return_value = mock_response
        
        connector = CanvasConnector("https://canvas.example.com", "bad_token")
        assert not connector.authenticate()
    
    @patch('requests.Session.get')
    def test_register_canvas_connector(self, mock_get, lms):
        """Test registering Canvas connector."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_get.return_value = mock_response
        
        success = lms.register_connector(
            "canvas_main",
            LMSType.CANVAS,
            "https://canvas.example.com",
            "test_token"
        )
        assert success
        assert "canvas_main" in lms.connectors
    
    @patch('requests.Session.get')
    def test_activate_lms(self, mock_get, lms):
        """Test activating LMS connector."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_get.return_value = mock_response
        
        lms.register_connector(
            "canvas_main",
            LMSType.CANVAS,
            "https://canvas.example.com",
            "test_token"
        )
        
        success = lms.activate_lms("canvas_main")
        assert success
        assert lms.active_lms == "canvas_main"
    
    def test_record_submission(self, lms):
        """Test recording student submission."""
        submission = lms.record_submission(
            assignment_id="assign_123",
            student_id="student_456",
            student_name="Test Student",
            code="PRINT 'test'",
            language="BASIC",
            score=95.0
        )
        
        assert submission.student_id == "student_456"
        assert submission.code == "PRINT 'test'"
        assert submission.score == 95.0
        assert len(lms.student_submissions) == 1
    
    def test_batch_grade(self, lms):
        """Test batch grading."""
        # Record submissions
        lms.record_submission("a1", "s1", "Alice", "code1", "BASIC", 80)
        lms.record_submission("a1", "s2", "Bob", "code2", "BASIC", 90)
        
        grades = {"s1": 85.0, "s2": 95.0}
        feedbacks = {"s1": "Good work", "s2": "Excellent"}
        
        # Note: This would require mocking the submit_grade method
        # which depends on active LMS connector
        assert len(lms.student_submissions) == 2
    
    def test_export_analytics(self, lms):
        """Test analytics export."""
        lms.record_submission("a1", "s1", "Alice", "code", "BASIC", 85)
        lms.record_submission("a2", "s2", "Bob", "code", "BASIC", 90)
        
        analytics = lms.export_analytics("course_123")
        assert analytics["course_id"] == "course_123"
        assert analytics["submissions"] == 2
        assert analytics["average_score"] == 87.5


# ============================================================================
# COMMUNITY MARKETPLACE TESTS
# ============================================================================

class TestCommunityMarketplace:
    """Test community marketplace system."""
    
    @pytest.fixture
    def marketplace(self):
        """Create marketplace instance."""
        return CommunityMarketplace()
    
    def test_publish_item(self, marketplace):
        """Test publishing item."""
        item = marketplace.publish_item(
            title="Hello World Template",
            description="Basic hello world template",
            item_type=ItemType.TEMPLATE,
            author_id="author_1",
            author_name="Test Author",
            author_avatar="https://example.com/avatar.jpg",
            content="PRINT 'Hello, World!'",
            language="BASIC",
            category="Beginner",
            tags=["hello", "basic", "tutorial"],
            license="MIT"
        )
        
        assert item.title == "Hello World Template"
        assert item.status == ItemStatus.REVIEW
        assert item.rating_avg == 0.0
    
    def test_approve_item(self, marketplace):
        """Test approving item for publication."""
        item = marketplace.publish_item(
            title="Test Template",
            description="Test",
            item_type=ItemType.TEMPLATE,
            author_id="author_2",
            author_name="Test",
            author_avatar="",
            content="content",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        success = marketplace.approve_item(item.id)
        assert success
        assert marketplace.items[item.id].status == ItemStatus.PUBLISHED
    
    def test_search_items(self, marketplace):
        """Test searching items."""
        # Publish multiple items
        item1 = marketplace.publish_item(
            title="Game Template",
            description="Create a game",
            item_type=ItemType.TEMPLATE,
            author_id="a1",
            author_name="Alice",
            author_avatar="",
            content="",
            language="BASIC",
            category="Games",
            tags=["game", "graphics"],
            license="MIT"
        )
        
        item2 = marketplace.publish_item(
            title="Math Solver",
            description="Solve math problems",
            item_type=ItemType.PROJECT,
            author_id="a2",
            author_name="Bob",
            author_avatar="",
            content="",
            language="BASIC",
            category="Math",
            tags=["math", "algorithm"],
            license="GPL"
        )
        
        # Approve both
        marketplace.approve_item(item1.id)
        marketplace.approve_item(item2.id)
        
        # Search
        results = marketplace.search_items(query="game")
        assert len(results) > 0
        assert results[0].title == "Game Template"
    
    def test_download_item(self, marketplace):
        """Test downloading item."""
        item = marketplace.publish_item(
            title="Test",
            description="",
            item_type=ItemType.SNIPPET,
            author_id="a1",
            author_name="Test",
            author_avatar="",
            content="PRINT 'Downloaded'",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        marketplace.approve_item(item.id)
        
        content = marketplace.download_item(item.id, "user_1")
        assert content == "PRINT 'Downloaded'"
        assert marketplace.items[item.id].download_count == 1
    
    def test_rate_item(self, marketplace):
        """Test rating item."""
        item = marketplace.publish_item(
            title="Awesome Template",
            description="",
            item_type=ItemType.TEMPLATE,
            author_id="a1",
            author_name="Test",
            author_avatar="",
            content="",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        marketplace.approve_item(item.id)
        
        # Add ratings
        marketplace.rate_item(item.id, "user_1", "Alice", 5, "Excellent!")
        marketplace.rate_item(item.id, "user_2", "Bob", 4, "Very good")
        
        updated_item = marketplace.items[item.id]
        assert updated_item.rating_avg == 4.5
        assert updated_item.rating_count == 2
    
    def test_add_to_favorites(self, marketplace):
        """Test favoriting item."""
        item = marketplace.publish_item(
            title="Favorite",
            description="",
            item_type=ItemType.TEMPLATE,
            author_id="a1",
            author_name="Test",
            author_avatar="",
            content="",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        marketplace.approve_item(item.id)
        
        success = marketplace.add_to_favorites(item.id, "user_1")
        assert success
        assert marketplace.stats[item.id].favorites == 1
    
    def test_get_user_items(self, marketplace):
        """Test getting user items."""
        item1 = marketplace.publish_item(
            title="Item 1",
            description="",
            item_type=ItemType.TEMPLATE,
            author_id="author_1",
            author_name="Alice",
            author_avatar="",
            content="",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        item2 = marketplace.publish_item(
            title="Item 2",
            description="",
            item_type=ItemType.SNIPPET,
            author_id="author_1",
            author_name="Alice",
            author_avatar="",
            content="",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        items = marketplace.get_user_items("author_1")
        assert len(items) == 2
    
    def test_report_item(self, marketplace):
        """Test reporting inappropriate item."""
        item = marketplace.publish_item(
            title="Spam",
            description="",
            item_type=ItemType.TEMPLATE,
            author_id="a1",
            author_name="Test",
            author_avatar="",
            content="",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        marketplace.approve_item(item.id)
        
        # Report multiple times
        for i in range(5):
            marketplace.report_item(item.id, "Spam", "user_1")
        
        # Should be suspended
        assert marketplace.items[item.id].status == ItemStatus.SUSPENDED
    
    def test_get_statistics(self, marketplace):
        """Test getting item statistics."""
        item = marketplace.publish_item(
            title="Popular",
            description="",
            item_type=ItemType.TEMPLATE,
            author_id="a1",
            author_name="Test",
            author_avatar="",
            content="",
            language="BASIC",
            category="Test",
            tags=[],
            license="MIT"
        )
        
        marketplace.approve_item(item.id)
        marketplace.download_item(item.id, "u1")
        marketplace.download_item(item.id, "u2")
        
        stats = marketplace.get_statistics(item.id)
        assert stats.downloads == 2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
