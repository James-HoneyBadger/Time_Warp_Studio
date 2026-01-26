"""
Business Logic Services Layer
Implements room management, sync, and collaboration services
"""

import logging
import uuid
from datetime import datetime
from typing import List, Optional

from sqlalchemy.ext.asyncio import AsyncSession

from .models import DocumentSnapshot, Message, Operation, Room, RoomMember
from .repositories import (
    DocumentSnapshotRepository,
    MessageRepository,
    OperationRepository,
    RoomMemberRepository,
    RoomRepository,
)

logger = logging.getLogger(__name__)


class RoomService:
    """Service for room management"""

    def __init__(self, session: AsyncSession):
        self.session = session
        self.repo = RoomRepository(session)
        self.member_repo = RoomMemberRepository(session)

    async def create_room(
        self, name: str, owner_id: str, is_private: bool = False
    ) -> Room:
        """Create a new collaboration room"""
        room = await self.repo.create(
            id=str(uuid.uuid4()),
            name=name,
            owner_id=owner_id,
            is_private=is_private,
        )

        # Add owner as first member
        await self.member_repo.create(
            id=str(uuid.uuid4()),
            room_id=room.id,
            user_id=owner_id,
            user_name="Owner",
            role="admin",
        )

        logger.info("Created room: %s by {owner_id}", room.id)
        return room

    async def get_room(self, room_id: str) -> Optional[Room]:
        """Get room by ID"""
        return await self.repo.get(room_id)

    async def add_member(
        self,
        room_id: str,
        user_id: str,
        user_name: str,
        user_email: str | None = None,
    ) -> RoomMember:
        """Add member to room"""
        member = await self.member_repo.create(
            id=str(uuid.uuid4()),
            room_id=room_id,
            user_id=user_id,
            user_name=user_name,
            user_email=user_email,
        )
        logger.info("Added member %s to room {room_id}", user_id)
        return member

    async def remove_member(self, room_id: str, user_id: str) -> bool:
        """Remove member from room"""
        member = await self.member_repo.get_member(room_id, user_id)
        if member:
            await self.member_repo.delete(member.id)
            logger.info("Removed member %s from room {room_id}", user_id)
            return True
        return False

    async def get_room_members(self, room_id: str) -> List[RoomMember]:
        """Get all members in room"""
        return await self.member_repo.get_room_members(room_id)

    async def get_active_members(self, room_id: str) -> List[RoomMember]:
        """Get active members in room"""
        return await self.member_repo.get_active_members(room_id)

    async def get_user_rooms(self, user_id: str) -> List[Room]:
        """Get all rooms user is a member of"""
        return await self.repo.get_user_rooms(user_id)

    async def delete_room(self, room_id: str) -> bool:
        """Delete room and all related data"""
        return await self.repo.delete(room_id)


class SyncService:
    """Service for operational transform synchronization"""

    def __init__(self, session: AsyncSession):
        self.session = session
        self.op_repo = OperationRepository(session)
        self.snapshot_repo = DocumentSnapshotRepository(session)

    async def record_operation(
        self,
        room_id: str,
        user_id: str,
        op_type: str,
        position: int,
        content: str,
    ) -> Operation:
        """Record an operation in history"""
        version = await self.op_repo.get_latest_version(room_id)
        version += 1

        operation = await self.op_repo.create(
            id=str(uuid.uuid4()),
            room_id=room_id,
            user_id=user_id,
            operation_type=op_type,
            position=position,
            content=content,
            version=version,
        )

        logger.info("Recorded operation v%s in room {room_id}", version)
        return operation

    async def get_operations_since(self, room_id: str, version: int) -> List[Operation]:
        """Get operations after a version"""
        return await self.op_repo.get_operations_since(room_id, version)

    async def get_latest_version(self, room_id: str) -> int:
        """Get latest operation version"""
        return await self.op_repo.get_latest_version(room_id)

    async def create_snapshot(
        self, room_id: str, content: str, version: int
    ) -> DocumentSnapshot:
        """Create a document snapshot for performance"""
        snapshot = await self.snapshot_repo.create(
            id=str(uuid.uuid4()),
            room_id=room_id,
            content=content,
            version=version,
            size_bytes=len(content.encode("utf-8")),
        )
        logger.info("Created snapshot v%s for room {room_id}", version)
        return snapshot

    async def get_latest_snapshot(self, room_id: str) -> Optional[DocumentSnapshot]:
        """Get latest snapshot"""
        return await self.snapshot_repo.get_latest_snapshot(room_id)


class ChatService:
    """Service for chat message persistence"""

    def __init__(self, session: AsyncSession):
        self.session = session
        self.repo = MessageRepository(session)

    async def add_message(
        self, room_id: str, user_id: str, username: str, content: str
    ) -> Message:
        """Add a chat message"""
        message = await self.repo.create(
            id=str(uuid.uuid4()),
            room_id=room_id,
            user_id=user_id,
            username=username,
            content=content,
        )
        logger.info("Added message to room %s", room_id)
        return message

    async def edit_message(
        self, message_id: str, new_content: str
    ) -> Optional[Message]:
        """Edit a message"""
        message = await self.repo.get(message_id)
        if message:
            message.content = new_content
            message.is_edited = True
            message.edited_at = datetime.utcnow()
            await self.session.commit()
            logger.info("Edited message %s", message_id)
        return message

    async def delete_message(self, message_id: str) -> bool:
        """Delete a message"""
        message = await self.repo.get(message_id)
        if message:
            message.is_deleted = True
            await self.session.commit()
            logger.info("Deleted message %s", message_id)
            return True
        return False

    async def add_reaction(self, message_id: str, emoji: str, user_id: str) -> bool:
        """Add emoji reaction to message"""
        message = await self.repo.get(message_id)
        if message:
            if emoji not in message.reactions:
                message.reactions[emoji] = []
            if user_id not in message.reactions[emoji]:
                message.reactions[emoji].append(user_id)
            await self.session.commit()
            return True
        return False

    async def get_room_messages(
        self, room_id: str, limit: int = 50, offset: int = 0
    ) -> List[Message]:
        """Get messages in room"""
        return await self.repo.get_room_messages(room_id, limit, offset)

    async def search_messages(self, room_id: str, query: str) -> List[Message]:
        """Search messages in room"""
        return await self.repo.search_messages(room_id, query)
