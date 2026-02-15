"""
Integration tests for Phase 4.5 Multiplayer Features
Tests WebSocket communication, OT algorithm, presence tracking, and chat
"""

# pylint: disable=import-error,redefined-outer-name

import asyncio
from unittest.mock import AsyncMock

import pytest

from time_warp.core.chat_service import ChatService  # type: ignore[import-not-found]
from time_warp.core.collaboration_engine import Operation, OperationalTransform  # type: ignore[import-not-found]
from time_warp.core.presence_service import PresenceService  # type: ignore[import-not-found]

from time_warp.core.websocket_manager import ConnectionManager  # type: ignore[import-not-found]


class TestWebSocketManager:
    """Test WebSocket connection management"""

    def test_connect_user(self):
        """Test user connection"""
        manager = ConnectionManager()
        websocket = AsyncMock()
        user_data = {"name": "Alice", "email": "alice@example.com"}

        asyncio.run(manager.connect("conn1", websocket, user_data))

        assert "conn1" in manager.active_connections
        assert manager.users["conn1"]["name"] == "Alice"

    def test_disconnect_user(self):
        """Test user disconnection"""
        manager = ConnectionManager()
        websocket = AsyncMock()
        user_data = {"name": "Bob"}

        asyncio.run(manager.connect("conn1", websocket, user_data))
        manager.disconnect("conn1")

        assert "conn1" not in manager.active_connections
        assert "conn1" not in manager.users

    def test_join_room(self):
        """Test joining a room"""
        manager = ConnectionManager()
        websocket = AsyncMock()
        user_data = {"name": "Charlie"}

        asyncio.run(manager.connect("conn1", websocket, user_data))
        asyncio.run(manager.join_room("conn1", "room1"))

        assert "room1" in manager.rooms
        assert "conn1" in manager.rooms["room1"]

    def test_leave_room(self):
        """Test leaving a room"""
        manager = ConnectionManager()
        websocket = AsyncMock()
        user_data = {"name": "Dave"}

        asyncio.run(manager.connect("conn1", websocket, user_data))
        asyncio.run(manager.join_room("conn1", "room1"))
        asyncio.run(manager.leave_room("conn1", "room1"))

        assert "room1" not in manager.rooms

    def test_broadcast_to_room(self):
        """Test broadcasting to room"""
        manager = ConnectionManager()
        websocket1 = AsyncMock()
        websocket2 = AsyncMock()

        asyncio.run(manager.connect("conn1", websocket1, {"name": "User1"}))
        asyncio.run(manager.connect("conn2", websocket2, {"name": "User2"}))
        asyncio.run(manager.join_room("conn1", "room1"))
        asyncio.run(manager.join_room("conn2", "room1"))

        message = {"type": "test", "content": "hello"}
        asyncio.run(manager.broadcast_to_room("room1", message))

        assert websocket1.send_json.called
        assert websocket2.send_json.called


class TestOperationalTransform:
    """Test Operational Transform algorithm"""

    def test_insert_operation(self):
        """Test insert operation"""
        ot = OperationalTransform("hello")
        op = ot.create_operation("user1", "insert", 5, " world")

        ot.apply_operation(op)

        assert ot.content == "hello world"
        assert ot.version == 1

    def test_delete_operation(self):
        """Test delete operation"""
        ot = OperationalTransform("hello world")
        op = ot.create_operation("user1", "delete", 5, " world")

        ot.apply_operation(op)

        assert ot.content == "hello"
        assert ot.version == 1

    def test_concurrent_inserts(self):
        """Test concurrent insert operations"""
        ot = OperationalTransform("ab")

        # User1 inserts at position 1
        op1 = ot.create_operation("user1", "insert", 1, "X")
        ot.apply_operation(op1)

        # User2 inserts at position 1 (concurrent, should transform)
        op2 = Operation(
            id="op2",
            user_id="user2",
            type="insert",
            position=1,
            content="Y",
            timestamp=op1.timestamp - 0.1,  # Concurrent
            version=op1.version,
        )

        # Transform op2 against op1
        transformed = ot._transform_operations(op2, op1)  # type: ignore[attr-defined]  # pylint: disable=protected-access

        # After transformation, position should be adjusted
        assert transformed.position == 2  # Shifted due to op1's insertion

    def test_conflict_detection(self):
        """Test conflict detection"""
        ot = OperationalTransform("test")

        op1 = ot.create_operation("user1", "insert", 2, "X")
        op2 = Operation(
            id="op2",
            user_id="user2",
            type="insert",
            position=2,
            content="Y",
            timestamp=op1.timestamp + 0.01,
            version=op1.version,
        )

        is_conflict, msg = ot.detect_conflict(op1, op2)

        assert is_conflict is True
        assert "concurrent" in msg.lower()

    def test_operation_history(self):
        """Test operation history tracking"""
        ot = OperationalTransform()

        op1 = ot.create_operation("user1", "insert", 0, "hello")
        op2 = ot.create_operation("user2", "insert", 5, " world")

        ot.apply_operation(op1)
        ot.apply_operation(op2)

        assert len(ot.operation_history) == 2
        assert ot.get_operations_since(0) == ot.operation_history


class TestPresenceService:
    """Test presence tracking"""

    def test_set_user_presence(self):
        """Test setting user presence"""
        service = PresenceService()
        user_data = {"name": "Alice", "color": "#FF0000"}

        presence = service.set_user_presence("conn1", "room1", user_data)

        assert presence["userId"] == "conn1"
        assert presence["name"] == "Alice"
        assert presence["status"] == "idle"

    def test_update_user_status(self):
        """Test updating user status"""
        service = PresenceService()
        user_data = {"name": "Bob"}

        service.set_user_presence("conn1", "room1", user_data)
        update = service.update_user_status("conn1", "room1", "editing")

        assert update["status"] == "editing"
        assert "timestamp" in update

    def test_update_cursor_position(self):
        """Test cursor position update"""
        service = PresenceService()
        user_data = {"name": "Charlie"}

        service.set_user_presence("conn1", "room1", user_data)
        position = {"line": 5, "column": 10}
        update = service.update_cursor_position("conn1", "room1", position)

        assert update["position"] == position

    def test_get_room_presence(self):
        """Test getting all presence data for room"""
        service = PresenceService()

        service.set_user_presence("conn1", "room1", {"name": "Alice"})
        service.set_user_presence("conn2", "room1", {"name": "Bob"})

        presence_list = service.get_room_presence("room1")

        assert len(presence_list) == 2
        assert presence_list[0]["name"] in ["Alice", "Bob"]

    def test_cleanup_inactive_users(self):
        """Test cleaning up inactive users"""
        service = PresenceService()

        service.set_user_presence("conn1", "room1", {"name": "Alice"})
        service.set_user_presence("conn2", "room1", {"name": "Bob"})

        # Simulate inactivity (this would need time manipulation in real test)
        removed = service.cleanup_inactive_users("room1", timeout_seconds=0)

        # All users should be removed if timeout is 0
        assert len(removed) == 2


class TestChatService:
    """Test chat messaging"""

    def test_add_message(self):
        """Test adding a message"""
        service = ChatService()

        message = service.add_message("room1", "user1", "Alice", "Hello!")

        assert message.content == "Hello!"
        assert message.username == "Alice"
        assert "room1" in service.messages

    def test_edit_message(self):
        """Test editing a message"""
        service = ChatService()

        message = service.add_message("room1", "user1", "Alice", "Hello!")
        message_id = message.id

        success = service.edit_message("room1", message_id, "Hello world!")

        assert success is True
        assert service.message_index["room1"][message_id].content == "Hello world!"

    def test_delete_message(self):
        """Test deleting a message"""
        service = ChatService()

        message = service.add_message("room1", "user1", "Alice", "Hello!")
        message_id = message.id

        success = service.delete_message("room1", message_id)

        assert success is True
        assert message_id not in service.message_index["room1"]

    def test_add_reaction(self):
        """Test adding emoji reaction"""
        service = ChatService()

        message = service.add_message("room1", "user1", "Alice", "Hello!")
        message_id = message.id

        success = service.add_reaction("room1", message_id, "👍", "user2")

        assert success is True
        assert "👍" in service.message_index["room1"][message_id].reactions

    def test_search_messages(self):
        """Test message search"""
        service = ChatService()

        service.add_message("room1", "user1", "Alice", "Hello world")
        service.add_message("room1", "user2", "Bob", "Goodbye")
        service.add_message("room1", "user1", "Alice", "Hello again")

        results = service.search_messages("room1", "hello")

        assert len(results) == 2

    def test_message_stats(self):
        """Test message statistics"""
        service = ChatService()

        service.add_message("room1", "user1", "Alice", "msg1")
        service.add_message("room1", "user2", "Bob", "msg2")
        service.add_message("room1", "user1", "Alice", "msg3")

        stats = service.get_message_stats("room1")

        assert stats["totalMessages"] == 3
        assert stats["uniqueUsers"] == 2
        assert stats["messagesByUser"]["Alice"] == 2


# Integration test scenarios
class TestMultiUserCollaboration:
    """Test multi-user collaboration scenarios"""

    def test_two_users_editing(self):
        """Test two users editing same document"""
        ot = OperationalTransform("hello")

        # User1 inserts at end
        op1 = ot.create_operation("user1", "insert", 5, " world")
        ot.apply_operation(op1)

        # User2 inserts in middle (concurrent)
        op2 = Operation(
            id="op2",
            user_id="user2",
            type="insert",
            position=2,
            content="XX",
            timestamp=op1.timestamp - 0.1,
            version=0,
        )

        ot.apply_operation(op2, transform_against=[op1])

        # Both operations should be applied
        assert len(ot.operation_history) == 2
        assert "XX" in ot.content

    def test_presence_awareness(self):
        """Test user presence awareness"""
        presence = PresenceService()

        # Two users join
        presence.set_user_presence("conn1", "room1", {"name": "Alice"})
        presence.set_user_presence("conn2", "room1", {"name": "Bob"})

        # Alice starts editing
        presence.update_user_status("conn1", "room1", "editing")

        # Bob sees Alice's status
        room_users = presence.get_room_presence("room1")

        alice = next((u for u in room_users if u["name"] == "Alice"), None)
        assert alice["status"] == "editing"

    def test_chat_and_code_together(self):
        """Test chat and code editing together"""
        chat = ChatService()
        ot = OperationalTransform()

        # Users chat
        chat.add_message("room1", "user1", "Alice", "Let's add a feature")
        chat.add_message("room1", "user2", "Bob", "Sure!")

        # Users edit code
        op1 = ot.create_operation("user1", "insert", 0, "def hello():")
        op2 = ot.create_operation("user2", "insert", 14, "\n  return 'hi'")

        ot.apply_operation(op1)
        ot.apply_operation(op2)

        # Both operations succeed
        assert len(chat.messages["room1"]) == 2
        assert len(ot.operation_history) == 2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
