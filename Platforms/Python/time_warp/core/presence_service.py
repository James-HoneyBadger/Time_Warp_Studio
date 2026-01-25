"""
Presence Service for Real-time User Status Tracking
Tracks user status, cursor position, and activity in rooms
"""

import logging
from datetime import datetime
from typing import Any, Dict, List

logger = logging.getLogger(__name__)


class PresenceService:
    """Manages user presence information in collaborative rooms"""

    def __init__(self):
        # User presence: {connection_id: presence_data}
        self.user_presence: Dict[str, Dict[str, Any]] = {}

        # Room presence: {room_id: {connection_id: presence_data}}
        self.room_presence: Dict[str, Dict[str, Any]] = {}

        # Typing indicators: {room_id: {connection_id: is_typing}}
        self.typing_indicators: Dict[str, Dict[str, bool]] = {}

        # Cursor positions: {room_id: {connection_id: position}}
        self.cursor_positions: Dict[str, Dict[str, Dict[str, int]]] = {}

    def set_user_presence(
        self,
        connection_id: str,
        room_id: str,
        user_data: Dict[str, Any],
    ) -> Dict[str, Any]:
        """Set presence data for a user"""
        if room_id not in self.room_presence:
            self.room_presence[room_id] = {}

        presence_data = {
            "connectionId": connection_id,
            "userId": user_data.get("id", connection_id),
            "name": user_data.get("name", "Anonymous"),
            "email": user_data.get("email", ""),
            "color": user_data.get("color", "#3B82F6"),
            "status": "idle",
            "cursorPosition": {"line": 0, "column": 0},
            "lastActivity": datetime.utcnow().isoformat(),
            "joinedAt": datetime.utcnow().isoformat(),
        }

        self.room_presence[room_id][connection_id] = presence_data
        self.user_presence[connection_id] = presence_data

        logger.info(f"User {user_data.get('name')} presence set in room {room_id}")
        return presence_data

    def update_user_status(
        self, connection_id: str, room_id: str, status: str
    ) -> Dict[str, Any]:
        """Update user status (idle, editing, running, away)"""
        if (
            room_id not in self.room_presence
            or connection_id not in self.room_presence[room_id]
        ):
            return {}

        presence = self.room_presence[room_id][connection_id]
        presence["status"] = status
        presence["lastActivity"] = datetime.utcnow().isoformat()

        self.user_presence[connection_id] = presence

        return {
            "type": "presence_update",
            "userId": presence.get("userId"),
            "status": status,
            "timestamp": presence["lastActivity"],
        }

    def update_cursor_position(
        self, connection_id: str, room_id: str, position: Dict[str, int]
    ) -> Dict[str, Any]:
        """Update user cursor position in editor"""
        if (
            room_id not in self.room_presence
            or connection_id not in self.room_presence[room_id]
        ):
            return {}

        presence = self.room_presence[room_id][connection_id]
        presence["cursorPosition"] = position
        presence["lastActivity"] = datetime.utcnow().isoformat()

        if room_id not in self.cursor_positions:
            self.cursor_positions[room_id] = {}

        self.cursor_positions[room_id][connection_id] = position

        return {
            "type": "cursor_update",
            "userId": presence.get("userId"),
            "position": position,
            "timestamp": presence["lastActivity"],
        }

    def set_typing(self, connection_id: str, room_id: str, is_typing: bool):
        """Set typing indicator for user"""
        if room_id not in self.typing_indicators:
            self.typing_indicators[room_id] = {}

        self.typing_indicators[room_id][connection_id] = is_typing

        return {
            "type": "typing_update",
            "userId": (
                self.user_presence.get(connection_id, {}).get("userId", connection_id)
            ),
            "isTyping": is_typing,
        }

    def get_room_presence(self, room_id: str) -> List[Dict[str, Any]]:
        """Get all presence data for users in a room"""
        if room_id not in self.room_presence:
            return []

        return list(self.room_presence[room_id].values())

    def get_user_presence(self, connection_id: str) -> Dict[str, Any]:
        """Get presence data for specific user"""
        return self.user_presence.get(connection_id, {})

    def remove_user_presence(self, connection_id: str, room_id: str = None):
        """Remove presence data for user"""
        if connection_id in self.user_presence:
            del self.user_presence[connection_id]

        if room_id and room_id in self.room_presence:
            if connection_id in self.room_presence[room_id]:
                del self.room_presence[room_id][connection_id]

        if room_id and room_id in self.typing_indicators:
            if connection_id in self.typing_indicators[room_id]:
                del self.typing_indicators[room_id][connection_id]

        if room_id and room_id in self.cursor_positions:
            if connection_id in self.cursor_positions[room_id]:
                del self.cursor_positions[room_id][connection_id]

        logger.info(f"Presence removed for {connection_id}")

    def get_typing_users(self, room_id: str) -> List[str]:
        """Get list of users currently typing in room"""
        if room_id not in self.typing_indicators:
            return []

        return [
            uid
            for uid, is_typing in self.typing_indicators[room_id].items()
            if is_typing
        ]

    def get_cursors(self, room_id: str) -> Dict[str, Dict[str, int]]:
        """Get all cursor positions in room"""
        return self.cursor_positions.get(room_id, {})

    def cleanup_inactive_users(self, room_id: str, timeout_seconds: int = 300):
        """Remove users inactive for longer than timeout"""
        if room_id not in self.room_presence:
            return []

        removed_users = []
        current_time = datetime.utcnow()

        for connection_id, presence in list(self.room_presence[room_id].items()):
            last_activity = datetime.fromisoformat(presence["lastActivity"])
            elapsed = (current_time - last_activity).total_seconds()

            if elapsed > timeout_seconds:
                removed_users.append(
                    {
                        "userId": presence.get("userId"),
                        "name": presence.get("name"),
                        "connectionId": connection_id,
                    }
                )
                self.remove_user_presence(connection_id, room_id)

        if removed_users:
            logger.info(f"Cleaned up {
                    len(removed_users)} inactive users from {room_id}")

        return removed_users

    def get_room_activity_summary(self, room_id: str) -> Dict[str, Any]:
        """Get summary of activity in a room"""
        if room_id not in self.room_presence:
            return {}

        presence_list = self.room_presence[room_id]
        status_counts = {"editing": 0, "idle": 0, "running": 0, "away": 0}

        for presence in presence_list.values():
            status = presence.get("status", "idle")
            status_counts[status] = status_counts.get(status, 0) + 1

        return {
            "roomId": room_id,
            "userCount": len(presence_list),
            "statusCounts": status_counts,
            "typingCount": len(self.get_typing_users(room_id)),
        }
