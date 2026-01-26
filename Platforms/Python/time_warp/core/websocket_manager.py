"""
WebSocket Manager for Real-time Collaboration
Handles connections, rooms, and message broadcasting
"""

import logging
from datetime import datetime
from typing import Any, Dict, List

from fastapi import WebSocket

logger = logging.getLogger(__name__)


class ConnectionManager:
    """Manages WebSocket connections and room subscriptions"""

    def __init__(self):
        # Active connections: {connection_id: websocket}
        self.active_connections: Dict[str, WebSocket] = {}

        # User info: {connection_id: user_data}
        self.users: Dict[str, Dict[str, Any]] = {}

        # Rooms: {room_id: {user_ids}}
        self.rooms: Dict[str, set] = {}

        # Room info: {room_id: room_data}
        self.room_info: Dict[str, Dict[str, Any]] = {}

        # Message history: {room_id: [messages]}
        self.message_history: Dict[str, List[Dict]] = {}

        # Max messages to keep per room
        self.max_history = 100

    async def connect(
        self,
        connection_id: str,
        websocket: WebSocket,
        user_data: Dict[str, Any],
    ):
        """Register a new connection"""
        await websocket.accept()
        self.active_connections[connection_id] = websocket
        self.users[connection_id] = user_data
        logger.info("User %s connected: {connection_id}", user_data.get('name'))

    def disconnect(self, connection_id: str):
        """Remove a connection"""
        user_data = self.users.get(connection_id, {})

        if connection_id in self.active_connections:
            self.active_connections.pop(connection_id)

        if connection_id in self.users:
            self.users.pop(connection_id)

        # Remove from all rooms
        for room_id, members in list(self.rooms.items()):
            if connection_id in members:
                members.discard(connection_id)
                if not members:
                    self.rooms.pop(room_id)

        if user_data:
            logger.info("User %s disconnected: {connection_id}", user_data.get('name'))

    async def join_room(self, connection_id: str, room_id: str):
        """Add connection to a room"""
        if room_id not in self.rooms:
            self.rooms[room_id] = set()
            self.message_history[room_id] = []

        self.rooms[room_id].add(connection_id)

        # Notify others in room
        await self.broadcast_to_room(
            room_id,
            {
                "type": "user_joined",
                "userId": connection_id,
                "name": self.users[connection_id].get("name"),
                "color": self.users[connection_id].get("color"),
                "timestamp": datetime.utcnow().isoformat(),
            },
            exclude_connection=connection_id,
        )

        # Send room info and existing users to new user
        room_members = [
            {
                "id": uid,
                "name": self.users[uid].get("name"),
                "color": self.users[uid].get("color"),
                "status": "idle",
                "cursorPosition": {"line": 0, "column": 0},
            }
            for uid in self.rooms[room_id]
            if uid != connection_id
        ]

        await self.send_to_connection(
            connection_id,
            {
                "type": "room_info",
                "id": room_id,
                "name": f"Room {room_id[:8]}",
                "privacy": "private",
                "participants": room_members,
            },
        )

        logger.info("Connection %s joined room {room_id}", connection_id)

    async def leave_room(self, connection_id: str, room_id: str):
        """Remove connection from a room"""
        if room_id in self.rooms:
            self.rooms[room_id].discard(connection_id)

            # Notify others
            user_data = self.users.get(connection_id, {})
            await self.broadcast_to_room(
                room_id,
                {
                    "type": "user_left",
                    "userId": connection_id,
                    "name": user_data.get("name"),
                    "timestamp": datetime.utcnow().isoformat(),
                },
            )

            if not self.rooms[room_id]:
                self.rooms.pop(room_id)
                if room_id in self.message_history:
                    self.message_history.pop(room_id)

        logger.info("Connection %s left room {room_id}", connection_id)

    async def send_to_connection(self, connection_id: str, message: Dict[str, Any]):
        """Send message to specific connection"""
        if connection_id in self.active_connections:
            try:
                await self.active_connections[connection_id].send_json(message)
            except Exception as e:
                logger.error("Error sending to %s: {e}", connection_id)

    async def broadcast_to_room(
        self,
        room_id: str,
        message: Dict[str, Any],
        exclude_connection: str | None = None,
    ):
        """Broadcast message to all users in a room"""
        if room_id not in self.rooms:
            return

        # Store in history if it's a chat or code change
        if message.get("type") in ["chat_message", "code_change"]:
            self.message_history[room_id].append(message)
            # Keep only last N messages
            if len(self.message_history[room_id]) > self.max_history:
                self.message_history[room_id].pop(0)

        dead_connections = []
        for connection_id in self.rooms[room_id]:
            if exclude_connection and connection_id == exclude_connection:
                continue

            try:
                await self.active_connections[connection_id].send_json(message)
            except Exception as e:
                logger.error("Error broadcasting to %s: {e}", connection_id)
                dead_connections.append(connection_id)

        # Clean up dead connections
        for connection_id in dead_connections:
            self.disconnect(connection_id)

    async def broadcast_to_all(self, message: Dict[str, Any]):
        """Broadcast message to all connected users"""
        dead_connections = []
        for connection_id in self.active_connections:
            try:
                await self.active_connections[connection_id].send_json(message)
            except Exception as e:
                logger.error("Error broadcasting to %s: {e}", connection_id)
                dead_connections.append(connection_id)

        for connection_id in dead_connections:
            self.disconnect(connection_id)

    def get_room_users(self, room_id: str) -> List[Dict[str, Any]]:
        """Get all users in a room with their info"""
        if room_id not in self.rooms:
            return []

        return [
            {
                "id": uid,
                "name": self.users[uid].get("name"),
                "email": self.users[uid].get("email"),
                "color": self.users[uid].get("color"),
            }
            for uid in self.rooms[room_id]
        ]

    def get_connection_info(self, connection_id: str) -> Dict[str, Any]:
        """Get info about a connection"""
        return self.users.get(connection_id, {})

    def is_user_in_room(self, room_id: str, connection_id: str) -> bool:
        """Check if user is in room"""
        return room_id in self.rooms and connection_id in self.rooms[room_id]

    def get_user_rooms(self, connection_id: str) -> List[str]:
        """Get all rooms a user is in"""
        return [
            room_id
            for room_id, members in self.rooms.items()
            if connection_id in members
        ]
