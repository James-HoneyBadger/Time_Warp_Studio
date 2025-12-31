"""
WebSocket Integration Tests
Tests real-time collaboration features
"""

import json
from unittest.mock import AsyncMock, Mock, patch

import pytest
from services import ChatService, RoomService, SyncService
from socketio import AsyncClient as SIOAsyncClient
from websocket_handlers import WebSocketEventHandler


@pytest.fixture
def mock_session():
    """Mock database session"""
    return AsyncMock()


@pytest.fixture
def mock_sio():
    """Mock Socket.io server"""
    return AsyncMock()


@pytest.fixture
def event_handler(mock_sio, mock_session):
    """Create event handler instance"""
    handler = WebSocketEventHandler(mock_sio, mock_session)
    return handler


@pytest.fixture
async def mock_room():
    """Mock room object"""
    room = Mock()
    room.id = "room123"
    room.name = "Test Room"
    room.owner_id = "user123"
    return room


# Connection Tests
@pytest.mark.asyncio
async def test_on_connect(event_handler):
    """Test connection handler"""
    await event_handler.on_connect("sid123", None)
    # Verify connection was registered
    assert "sid123" in event_handler.connections


@pytest.mark.asyncio
async def test_on_disconnect(event_handler):
    """Test disconnection handler"""
    # Register connection first
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    await event_handler.on_disconnect("sid123")

    # Verify connection was cleaned up
    assert "sid123" not in event_handler.connections


# Room Collaboration Tests
@pytest.mark.asyncio
async def test_on_join_room(event_handler, mock_sio, mock_session):
    """Test join room handler"""
    with patch.object(
        RoomService, "add_member", new_callable=AsyncMock
    ) as mock_add:
        mock_member = Mock()
        mock_member.id = "member123"
        mock_add.return_value = mock_member

        await event_handler.on_join_room(
            "sid123",
            {"room_id": "room123", "user_id": "user123", "username": "John"},
        )

        # Verify service was called
        mock_add.assert_called_once()
        # Verify connection was registered
        assert event_handler.connections["sid123"]["room_id"] == "room123"


@pytest.mark.asyncio
async def test_on_leave_room(event_handler, mock_sio, mock_session):
    """Test leave room handler"""
    # Register connection first
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    with patch.object(
        RoomService, "remove_member", new_callable=AsyncMock
    ) as mock_remove:
        mock_remove.return_value = True

        await event_handler.on_leave_room("sid123", {})

        # Verify service was called
        mock_remove.assert_called_once()


# Code Collaboration Tests
@pytest.mark.asyncio
async def test_on_code_change(event_handler, mock_sio, mock_session):
    """Test code change handler (OT)"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    with patch.object(
        SyncService, "record_operation", new_callable=AsyncMock
    ) as mock_record:
        mock_operation = Mock()
        mock_operation.id = "op123"
        mock_operation.version = 1
        mock_record.return_value = mock_operation

        await event_handler.on_code_change(
            "sid123",
            {
                "room_id": "room123",
                "user_id": "user123",
                "op_type": "insert",
                "position": 0,
                "content": "hello",
            },
        )

        # Verify operation was recorded
        mock_record.assert_called_once()


@pytest.mark.asyncio
async def test_on_cursor_update(event_handler, mock_sio):
    """Test cursor update handler"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    await event_handler.on_cursor_update(
        "sid123", {"room_id": "room123", "position": 42}
    )

    # Verify broadcast was called
    mock_sio.emit.assert_called()


@pytest.mark.asyncio
async def test_on_typing(event_handler, mock_sio):
    """Test typing indicator handler"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    await event_handler.on_typing(
        "sid123", {"room_id": "room123", "is_typing": True}
    )

    # Verify broadcast was called
    mock_sio.emit.assert_called()


# Chat Tests
@pytest.mark.asyncio
async def test_on_chat_message(event_handler, mock_sio, mock_session):
    """Test chat message handler"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    with patch.object(
        ChatService, "add_message", new_callable=AsyncMock
    ) as mock_add:
        mock_message = Mock()
        mock_message.id = "msg123"
        mock_add.return_value = mock_message

        await event_handler.on_chat_message(
            "sid123", {"room_id": "room123", "content": "Hello everyone!"}
        )

        # Verify message was saved
        mock_add.assert_called_once()
        # Verify broadcast was called
        mock_sio.emit.assert_called()


@pytest.mark.asyncio
async def test_on_add_reaction(event_handler, mock_sio, mock_session):
    """Test reaction handler"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    with patch.object(
        ChatService, "add_reaction", new_callable=AsyncMock
    ) as mock_reaction:
        await event_handler.on_add_reaction(
            "sid123",
            {"room_id": "room123", "message_id": "msg123", "emoji": "👍"},
        )

        # Verify reaction was saved
        mock_reaction.assert_called_once()


# Sync Tests
@pytest.mark.asyncio
async def test_on_get_sync(event_handler, mock_sio, mock_session):
    """Test sync request handler"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    with patch.object(
        SyncService, "get_operations_since", new_callable=AsyncMock
    ) as mock_get:
        mock_get.return_value = []

        await event_handler.on_get_sync(
            "sid123", {"room_id": "room123", "version": 0}
        )

        # Verify sync was retrieved
        mock_get.assert_called_once()
        # Verify response was emitted
        mock_sio.emit.assert_called()


# Presence Tests
@pytest.mark.asyncio
async def test_on_presence_update(event_handler, mock_sio):
    """Test presence update handler"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    await event_handler.on_presence_update(
        "sid123",
        {"room_id": "room123", "status": "away"},
    )

    # Verify broadcast was called
    mock_sio.emit.assert_called()


# Error Handling Tests
@pytest.mark.asyncio
async def test_on_code_change_invalid_data(event_handler):
    """Test code change with invalid data"""
    # Register connection
    event_handler.connections["sid123"] = {
        "user_id": "user123",
        "room_id": "room123",
    }

    # Should not raise exception with invalid data
    result = await event_handler.on_code_change(
        "sid123", {"invalid": "data"}  # Missing required fields
    )

    # Handler should handle gracefully
    assert result is None or isinstance(result, dict)


@pytest.mark.asyncio
async def test_unregistered_connection(event_handler):
    """Test handling of unregistered connection"""
    # Connection not in registry
    assert "unknown_sid" not in event_handler.connections

    # Should handle gracefully
    result = await event_handler.on_cursor_update(
        "unknown_sid", {"position": 0}
    )

    assert result is None or isinstance(result, dict)
