"""
WebSocket Event Handlers for Socket.io
Processes incoming WebSocket events and manages collaboration
"""

import logging
from typing import Any, Dict

from socketio import AsyncServer

from .core.chat_service import ChatService as ChatServiceMemory
from .core.collaboration_engine import OperationalTransform
from .core.presence_service import PresenceService
from .core.websocket_manager import ConnectionManager
from .db import AsyncSessionLocal
from .services import ChatService, RoomService, SyncService

logger = logging.getLogger(__name__)


class WebSocketEventHandler:
    """Handles Socket.io events for real-time collaboration"""

    def __init__(self, sio: AsyncServer):
        self.sio = sio
        self.connection_manager = ConnectionManager()
        self.presence_service = PresenceService()
        self.chat_service_memory = ChatServiceMemory()

        # In-memory OT engines per room
        self.ot_engines: Dict[str, OperationalTransform] = {}

    def register_all(self):
        """Register all event handlers"""
        self.sio.on("connect", self.on_connect)
        self.sio.on("disconnect", self.on_disconnect)
        self.sio.on("join_room", self.on_join_room)
        self.sio.on("leave_room", self.on_leave_room)
        self.sio.on("code_change", self.on_code_change)
        self.sio.on("cursor_update", self.on_cursor_update)
        self.sio.on("presence_update", self.on_presence_update)
        self.sio.on("typing", self.on_typing)
        self.sio.on("chat_message", self.on_chat_message)
        self.sio.on("add_reaction", self.on_add_reaction)
        self.sio.on("get_sync", self.on_get_sync)

        logger.info("WebSocket event handlers registered")

    async def on_connect(self, sid: str, environ: Dict[str, Any]):
        """Handle connection"""
        logger.info("Client connected: %s", sid)
        await self.sio.emit(
            "connection_response", {"data": "Connected to server"}, to=sid
        )

    async def on_disconnect(self, sid: str):
        """Handle disconnection"""
        logger.info("Client disconnected: %s", sid)
        # Clean up user from all rooms
        rooms = self.connection_manager.get_user_rooms(sid)
        for room_id in rooms:
            await self.on_leave_room(sid, {"room_id": room_id})

        # Remove from connection manager
        self.connection_manager.disconnect(sid)

    async def on_join_room(self, sid: str, data: Dict[str, Any]):
        """Handle room join"""
        room_id = data.get("room_id")
        user_id = data.get("user_id")
        user_data = data.get("user_data", {})

        if not room_id or not user_id:
            logger.warning("Invalid join_room data")
            return

        # Add to connection manager
        user_data["id"] = user_id
        # Ensure user is tracked in manager
        if sid not in self.connection_manager.users:
            self.connection_manager.users[sid] = user_data

        await self.connection_manager.join_room(sid, room_id)
        await self.sio.enter_room(sid, room_id)

        # Add to database
        try:
            async with AsyncSessionLocal() as session:
                room_service = RoomService(session)
                await room_service.add_member(
                    room_id,
                    user_id,
                    user_data.get("name", "Anonymous"),
                    user_data.get("email"),
                )
        except Exception as e:
            logger.error("Error adding member to database: %s", e)

        # Set presence
        self.presence_service.set_user_presence(sid, room_id, user_data)

        # Initialize OT engine if needed
        if room_id not in self.ot_engines:
            self.ot_engines[room_id] = OperationalTransform()

        # Notify others
        await self.sio.emit(
            "user_joined",
            {
                "user_id": user_id,
                "name": user_data.get("name"),
                "color": user_data.get("color"),
            },
            to=room_id,
            skip_sid=sid,
        )

        logger.info("User %s joined room {room_id}", user_id)

    async def on_leave_room(self, sid: str, data: Dict[str, Any]):
        """Handle room leave"""
        room_id = data.get("room_id")
        if not room_id:
            return

        user_data = self.presence_service.get_user_presence(sid)
        user_id = user_data.get("userId", "unknown")

        # Remove from connection manager
        await self.connection_manager.leave_room(sid, room_id)

        # Remove presence
        self.presence_service.remove_user_presence(sid, room_id)

        # Notify others
        await self.sio.emit(
            "user_left",
            {"user_id": user_id, "name": user_data.get("name")},
            to=room_id,
        )

        logger.info("User %s left room {room_id}", user_id)

    async def on_code_change(self, sid: str, data: Dict[str, Any]):
        """Handle code changes with OT"""
        room_id = data.get("room_id")
        user_id = data.get("user_id")

        if not room_id:
            return

        # Get OT engine
        ot = self.ot_engines.get(room_id)
        if not ot:
            ot = OperationalTransform()
            self.ot_engines[room_id] = ot

        # Create operation
        op = ot.create_operation(
            user_id,
            data.get("type", "insert"),
            data.get("position", 0),
            data.get("content", ""),
        )

        # Apply with transformation
        success, message = ot.apply_operation(op)

        if success:
            # Persist to database
            try:
                async with AsyncSessionLocal() as session:
                    sync_service = SyncService(session)
                    await sync_service.record_operation(
                        room_id,
                        user_id,
                        op.type,
                        op.position,
                        op.content,
                    )
            except Exception as e:
                logger.error("Error persisting operation: %s", e)

            # Broadcast to room
            await self.sio.emit(
                "code_change",
                {
                    "id": op.id,
                    "userId": user_id,
                    "type": op.type,
                    "position": op.position,
                    "content": op.content,
                    "version": ot.version,
                },
                to=room_id,
                skip_sid=sid,
            )

    async def on_cursor_update(self, sid: str, data: Dict[str, Any]):
        """Handle cursor position updates"""
        room_id = data.get("room_id")
        position = data.get("position")

        if not room_id:
            return

        # Update in presence service
        update = self.presence_service.update_cursor_position(sid, room_id, position)

        # Broadcast to room
        await self.sio.emit("cursor_update", update, to=room_id, skip_sid=sid)

    async def on_presence_update(self, sid: str, data: Dict[str, Any]):
        """Handle presence/status updates"""
        room_id = data.get("room_id")
        status = data.get("status")

        if not room_id:
            return

        update = self.presence_service.update_user_status(sid, room_id, status)
        await self.sio.emit("presence_update", update, to=room_id, skip_sid=sid)

    async def on_typing(self, sid: str, data: Dict[str, Any]):
        """Handle typing indicator"""
        room_id = data.get("room_id")
        is_typing = data.get("is_typing", False)

        if not room_id:
            return

        update = self.presence_service.set_typing(sid, room_id, is_typing)
        await self.sio.emit("typing", update, to=room_id, skip_sid=sid)

    async def on_chat_message(self, sid: str, data: Dict[str, Any]):
        """Handle chat message"""
        room_id = data.get("room_id")
        user_id = data.get("user_id")
        username = data.get("username", "Anonymous")
        content = data.get("content", "")

        if not room_id:
            return

        # Store in memory (fast)
        message = self.chat_service_memory.add_message(
            room_id, user_id, username, content
        )

        # Persist to database (async)
        try:
            async with AsyncSessionLocal() as session:
                chat_service = ChatService(session)
                await chat_service.add_message(room_id, user_id, username, content)
        except Exception as e:
            logger.error("Error persisting message: %s", e)

        # Broadcast
        await self.sio.emit(
            "chat_message",
            {
                "id": message.id,
                "user_id": user_id,
                "username": username,
                "content": content,
                "timestamp": message.timestamp,
            },
            to=room_id,
        )

    async def on_add_reaction(self, sid: str, data: Dict[str, Any]):
        """Handle message reaction"""
        room_id = data.get("room_id")
        message_id = data.get("message_id")
        emoji = data.get("emoji")
        user_id = data.get("user_id")

        if not room_id:
            return

        # Add to memory
        self.chat_service_memory.add_reaction(room_id, message_id, emoji, user_id)

        # Persist to database
        try:
            async with AsyncSessionLocal() as session:
                chat_service = ChatService(session)
                await chat_service.add_reaction(message_id, emoji, user_id)
        except Exception as e:
            logger.error("Error adding reaction: %s", e)

        # Broadcast
        await self.sio.emit(
            "reaction_added",
            {"message_id": message_id, "emoji": emoji, "user_id": user_id},
            to=room_id,
        )

    async def on_get_sync(self, sid: str, data: Dict[str, Any]):
        """Handle sync request"""
        room_id = data.get("room_id")
        version = data.get("version", 0)

        if not room_id:
            return

        ot = self.ot_engines.get(room_id)
        if not ot:
            ot = OperationalTransform()
            self.ot_engines[room_id] = ot

        operations = ot.get_operations_since(version)

        await self.sio.emit(
            "sync_response",
            {
                "version": ot.version,
                "operations": [op.to_dict() for op in operations],
                "content": ot.content,
            },
            to=sid,
        )
