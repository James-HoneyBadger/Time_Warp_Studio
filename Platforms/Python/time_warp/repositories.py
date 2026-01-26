"""
Repository Pattern Implementation for Data Access
Provides CRUD operations for all models with async support
"""

import logging
from typing import List, Optional

from sqlalchemy import and_, desc, select
from sqlalchemy.ext.asyncio import AsyncSession

from .models import (
    DocumentSnapshot,
    Message,
    Operation,
    Room,
    RoomMember,
)

logger = logging.getLogger(__name__)


class BaseRepository:
    """Base repository with common CRUD operations"""

    def __init__(self, session: AsyncSession, model):
        self.session = session
        self.model = model

    async def create(self, **kwargs) -> any:
        """Create new record"""
        instance = self.model(**kwargs)
        self.session.add(instance)
        await self.session.commit()
        logger.info("Created %s: {instance.id}", self.model.__name__)
        return instance

    async def get(self, id: str) -> Optional[any]:
        """Get record by ID"""
        result = await self.session.execute(
            select(self.model).where(self.model.id == id)
        )
        return result.scalars().first()

    async def get_all(self, limit: int = 100, offset: int = 0) -> List[any]:
        """Get all records with pagination"""
        result = await self.session.execute(
            select(self.model).offset(offset).limit(limit)
        )
        return result.scalars().all()

    async def update(self, id: str, **kwargs) -> Optional[any]:
        """Update record"""
        instance = await self.get(id)
        if instance:
            for key, value in kwargs.items():
                setattr(instance, key, value)
            await self.session.commit()
            logger.info("Updated %s: {id}", self.model.__name__)
        return instance

    async def delete(self, id: str) -> bool:
        """Delete record"""
        instance = await self.get(id)
        if instance:
            await self.session.delete(instance)
            await self.session.commit()
            logger.info("Deleted %s: {id}", self.model.__name__)
            return True
        return False


class RoomRepository(BaseRepository):
    """Repository for Room model"""

    def __init__(self, session: AsyncSession):
        super().__init__(session, Room)

    async def get_by_owner(self, owner_id: str) -> List[Room]:
        """Get all rooms owned by user"""
        result = await self.session.execute(
            select(self.model)
            .where(self.model.owner_id == owner_id)
            .order_by(desc(self.model.created_at))
        )
        return result.scalars().all()

    async def get_user_rooms(self, user_id: str) -> List[Room]:
        """Get all rooms user is member of"""
        result = await self.session.execute(
            select(self.model)
            .join(RoomMember)
            .where(RoomMember.user_id == user_id)
            .order_by(desc(self.model.updated_at))
        )
        return result.scalars().all()

    async def search(self, query: str) -> List[Room]:
        """Search rooms by name"""
        search_query = f"%{query}%"
        result = await self.session.execute(
            select(self.model).where(self.model.name.ilike(search_query))
        )
        return result.scalars().all()


class RoomMemberRepository(BaseRepository):
    """Repository for RoomMember model"""

    def __init__(self, session: AsyncSession):
        super().__init__(session, RoomMember)

    async def get_room_members(self, room_id: str) -> List[RoomMember]:
        """Get all members in a room"""
        result = await self.session.execute(
            select(self.model).where(self.model.room_id == room_id)
        )
        return result.scalars().all()

    async def get_active_members(self, room_id: str) -> List[RoomMember]:
        """Get active members in a room"""
        result = await self.session.execute(
            select(self.model).where(
                and_(self.model.room_id == room_id, self.model.is_active)
            )
        )
        return result.scalars().all()

    async def get_member(self, room_id: str, user_id: str) -> Optional[RoomMember]:
        """Get specific member"""
        result = await self.session.execute(
            select(self.model).where(
                and_(
                    self.model.room_id == room_id,
                    self.model.user_id == user_id,
                )
            )
        )
        return result.scalars().first()


class OperationRepository(BaseRepository):
    """Repository for Operation model (Operational Transform history)"""

    def __init__(self, session: AsyncSession):
        super().__init__(session, Operation)

    async def get_room_operations(
        self, room_id: str, limit: int = 100
    ) -> List[Operation]:
        """Get operations in a room"""
        result = await self.session.execute(
            select(self.model)
            .where(self.model.room_id == room_id)
            .order_by(self.model.version)
            .limit(limit)
        )
        return result.scalars().all()

    async def get_operations_since(self, room_id: str, version: int) -> List[Operation]:
        """Get operations after a specific version"""
        result = await self.session.execute(
            select(self.model)
            .where(and_(self.model.room_id == room_id, self.model.version > version))
            .order_by(self.model.version)
        )
        return result.scalars().all()

    async def get_latest_version(self, room_id: str) -> int:
        """Get latest operation version in room"""
        result = await self.session.execute(
            select(self.model.version)
            .where(self.model.room_id == room_id)
            .order_by(desc(self.model.version))
            .limit(1)
        )
        version = result.scalar()
        return version or 0

    async def get_user_operations(self, room_id: str, user_id: str) -> List[Operation]:
        """Get operations by specific user in room"""
        result = await self.session.execute(
            select(self.model)
            .where(
                and_(
                    self.model.room_id == room_id,
                    self.model.user_id == user_id,
                )
            )
            .order_by(self.model.timestamp)
        )
        return result.scalars().all()


class MessageRepository(BaseRepository):
    """Repository for Message model"""

    def __init__(self, session: AsyncSession):
        super().__init__(session, Message)

    async def get_room_messages(
        self, room_id: str, limit: int = 50, offset: int = 0
    ) -> List[Message]:
        """Get messages in a room with pagination"""
        result = await self.session.execute(
            select(self.model)
            .where(self.model.room_id == room_id)
            .order_by(desc(self.model.timestamp))
            .offset(offset)
            .limit(limit)
        )
        return result.scalars().all()

    async def search_messages(self, room_id: str, query: str) -> List[Message]:
        """Search messages in a room"""
        search_query = f"%{query}%"
        result = await self.session.execute(
            select(self.model)
            .where(
                and_(
                    self.model.room_id == room_id,
                    self.model.content.ilike(search_query),
                )
            )
            .order_by(desc(self.model.timestamp))
        )
        return result.scalars().all()

    async def get_user_messages(self, room_id: str, user_id: str) -> List[Message]:
        """Get messages by specific user"""
        result = await self.session.execute(
            select(self.model)
            .where(
                and_(
                    self.model.room_id == room_id,
                    self.model.user_id == user_id,
                )
            )
            .order_by(desc(self.model.timestamp))
        )
        return result.scalars().all()


class DocumentSnapshotRepository(BaseRepository):
    """Repository for DocumentSnapshot model"""

    def __init__(self, session: AsyncSession):
        super().__init__(session, DocumentSnapshot)

    async def get_latest_snapshot(self, room_id: str) -> Optional[DocumentSnapshot]:
        """Get latest snapshot for a room"""
        result = await self.session.execute(
            select(self.model)
            .where(self.model.room_id == room_id)
            .order_by(desc(self.model.version))
            .limit(1)
        )
        return result.scalars().first()

    async def get_snapshot_at_version(
        self, room_id: str, version: int
    ) -> Optional[DocumentSnapshot]:
        """Get snapshot at specific version"""
        result = await self.session.execute(
            select(self.model)
            .where(
                and_(
                    self.model.room_id == room_id,
                    self.model.version == version,
                )
            )
            .limit(1)
        )
        return result.scalars().first()
