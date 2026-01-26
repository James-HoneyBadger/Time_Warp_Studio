"""
Chat Service for Real-time Messaging
Manages chat messages, notifications, and message history
"""

import logging
import uuid
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List

logger = logging.getLogger(__name__)


@dataclass
class ChatMessage:
    """Represents a chat message"""

    id: str
    room_id: str
    user_id: str
    username: str
    content: str
    timestamp: float
    edited: bool = False
    edited_at: float | None = None
    reactions: Dict[str, List[str]] = None  # emoji -> [user_ids]

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "roomId": self.room_id,
            "userId": self.user_id,
            "username": self.username,
            "content": self.content,
            "timestamp": self.timestamp,
            "edited": self.edited,
            "editedAt": self.edited_at,
            "reactions": self.reactions or {},
        }


class ChatService:
    """Manages chat messages and room conversations"""

    def __init__(self, max_messages_per_room: int = 500):
        # Message history: {room_id: [messages]}
        self.messages: Dict[str, List[ChatMessage]] = {}

        # Message index: {room_id: {message_id: message}}
        self.message_index: Dict[str, Dict[str, ChatMessage]] = {}

        # Unread counts: {room_id: {user_id: count}}
        self.unread_counts: Dict[str, Dict[str, int]] = {}

        # Typing users: {room_id: set(user_ids)}
        self.typing_users: Dict[str, set] = {}

        self.max_messages = max_messages_per_room

    def add_message(
        self, room_id: str, user_id: str, username: str, content: str
    ) -> ChatMessage:
        """Add a message to a room"""
        if room_id not in self.messages:
            self.messages[room_id] = []
            self.message_index[room_id] = {}
            self.unread_counts[room_id] = {}

        message = ChatMessage(
            id=str(uuid.uuid4()),
            room_id=room_id,
            user_id=user_id,
            username=username,
            content=content,
            timestamp=datetime.utcnow().timestamp(),
        )

        self.messages[room_id].append(message)
        self.message_index[room_id][message.id] = message

        # Trim old messages if exceeded limit
        if len(self.messages[room_id]) > self.max_messages:
            old_message = self.messages[room_id].pop(0)
            del self.message_index[room_id][old_message.id]

        logger.info("Message added to room %s by {username}: {content[:50]}...", room_id)
        return message

    def edit_message(self, room_id: str, message_id: str, new_content: str) -> bool:
        """Edit an existing message"""
        if (
            room_id not in self.message_index
            or message_id not in self.message_index[room_id]
        ):
            return False

        message = self.message_index[room_id][message_id]
        message.content = new_content
        message.edited = True
        message.edited_at = datetime.utcnow().timestamp()

        return True

    def delete_message(self, room_id: str, message_id: str) -> bool:
        """Delete a message"""
        if (
            room_id not in self.message_index
            or message_id not in self.message_index[room_id]
        ):
            return False

        message = self.message_index[room_id][message_id]
        self.messages[room_id].remove(message)
        del self.message_index[room_id][message_id]

        return True

    def add_reaction(
        self, room_id: str, message_id: str, emoji: str, user_id: str
    ) -> bool:
        """Add emoji reaction to a message"""
        if (
            room_id not in self.message_index
            or message_id not in self.message_index[room_id]
        ):
            return False

        message = self.message_index[room_id][message_id]
        if message.reactions is None:
            message.reactions = {}

        if emoji not in message.reactions:
            message.reactions[emoji] = []

        if user_id not in message.reactions[emoji]:
            message.reactions[emoji].append(user_id)

        return True

    def remove_reaction(
        self, room_id: str, message_id: str, emoji: str, user_id: str
    ) -> bool:
        """Remove emoji reaction from a message"""
        if (
            room_id not in self.message_index
            or message_id not in self.message_index[room_id]
        ):
            return False

        message = self.message_index[room_id][message_id]
        if not message.reactions or emoji not in message.reactions:
            return False

        if user_id in message.reactions[emoji]:
            message.reactions[emoji].remove(user_id)
            if not message.reactions[emoji]:
                del message.reactions[emoji]

        return True

    def get_room_messages(
        self, room_id: str, limit: int = 50, offset: int = 0
    ) -> List[Dict[str, Any]]:
        """Get messages from a room with pagination"""
        if room_id not in self.messages:
            return []

        messages = self.messages[room_id]
        start = max(0, len(messages) - offset - limit)
        end = max(0, len(messages) - offset)

        return [msg.to_dict() for msg in messages[start:end]]

    def set_typing(self, room_id: str, user_id: str, is_typing: bool):
        """Update typing status for user"""
        if room_id not in self.typing_users:
            self.typing_users[room_id] = set()

        if is_typing:
            self.typing_users[room_id].add(user_id)
        else:
            self.typing_users[room_id].discard(user_id)

    def get_typing_users(self, room_id: str) -> List[str]:
        """Get users currently typing in room"""
        if room_id not in self.typing_users:
            return []

        return list(self.typing_users[room_id])

    def search_messages(
        self, room_id: str, query: str, limit: int = 50
    ) -> List[Dict[str, Any]]:
        """Search messages in a room"""
        if room_id not in self.messages:
            return []

        query_lower = query.lower()
        results = [
            msg.to_dict()
            for msg in self.messages[room_id]
            if query_lower in msg.content.lower()
        ]

        return results[-limit:]  # Return most recent matches

    def get_user_messages(
        self, room_id: str, user_id: str, limit: int = 50
    ) -> List[Dict[str, Any]]:
        """Get all messages from a specific user in a room"""
        if room_id not in self.messages:
            return []

        messages = [msg for msg in self.messages[room_id] if msg.user_id == user_id]

        return [msg.to_dict() for msg in messages[-limit:]]

    def get_message_stats(self, room_id: str) -> Dict[str, Any]:
        """Get statistics about messages in a room"""
        if room_id not in self.messages:
            return {}

        messages = self.messages[room_id]
        user_message_counts: Dict[str, int] = {}

        for msg in messages:
            user_message_counts[msg.username] = (
                user_message_counts.get(msg.username, 0) + 1
            )

        return {
            "roomId": room_id,
            "totalMessages": len(messages),
            "uniqueUsers": len(user_message_counts),
            "messagesByUser": user_message_counts,
            "oldestMessage": (messages[0].timestamp if messages else None),
            "newestMessage": messages[-1].timestamp if messages else None,
        }

    def clear_room_messages(self, room_id: str):
        """Clear all messages from a room"""
        if room_id in self.messages:
            self.messages[room_id] = []
            self.message_index[room_id] = {}
            logger.info("Cleared all messages from room %s", room_id)

    def export_messages(self, room_id: str, format: str = "json") -> str:
        """Export messages from a room"""
        if room_id not in self.messages:
            return ""

        messages = self.messages[room_id]

        if format == "json":
            import json

            return json.dumps([msg.to_dict() for msg in messages], indent=2)
        elif format == "csv":
            lines = ["timestamp,username,content"]
            for msg in messages:
                lines.append(f'{
                        msg.timestamp},"{
                        msg.username}","{
                        msg.content.replace(
                            chr(34),
                            chr(34) +
                            chr(34))}"')
            return "\n".join(lines)
        elif format == "txt":
            lines = []
            for msg in messages:
                time_str = datetime.fromtimestamp(msg.timestamp).strftime(
                    "%Y-%m-%d %H:%M:%S"
                )
                lines.append(f"[{time_str}] {msg.username}: {msg.content}")
            return "\n".join(lines)

        return ""
