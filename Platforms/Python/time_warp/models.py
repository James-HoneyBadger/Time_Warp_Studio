"""
SQLAlchemy Models for Collaborative Editing
Defines schema for rooms, messages, operations, and user presence
"""

from datetime import datetime

from sqlalchemy import (
    JSON,
    Boolean,
    Column,
    DateTime,
    ForeignKey,
    Index,
    Integer,
    String,
    Text,
)
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship

Base = declarative_base()


class Room(Base):
    """Collaboration room model"""

    __tablename__ = "rooms"

    id = Column(String(255), primary_key=True, index=True)
    name = Column(String(255), nullable=False)
    description = Column(Text, nullable=True)
    owner_id = Column(String(255), nullable=False, index=True)
    is_private = Column(Boolean, default=False)
    max_users = Column(Integer, default=100)
    created_at = Column(DateTime, default=datetime.utcnow, index=True)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relationships
    messages = relationship(
        "Message", back_populates="room", cascade="all, delete-orphan"
    )
    operations = relationship(
        "Operation", back_populates="room", cascade="all, delete-orphan"
    )
    members = relationship(
        "RoomMember", back_populates="room", cascade="all, delete-orphan"
    )

    def to_dict(self):
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "owner_id": self.owner_id,
            "is_private": self.is_private,
            "max_users": self.max_users,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
        }


class RoomMember(Base):
    """Room membership tracking"""

    __tablename__ = "room_members"
    __table_args__ = (Index("idx_room_user", "room_id", "user_id"),)

    id = Column(String(255), primary_key=True, index=True)
    room_id = Column(String(255), ForeignKey("rooms.id"), nullable=False, index=True)
    user_id = Column(String(255), nullable=False, index=True)
    user_name = Column(String(255), nullable=False)
    user_email = Column(String(255), nullable=True)
    user_color = Column(String(7), default="#3B82F6")
    role = Column(String(50), default="member")  # admin, member, viewer
    joined_at = Column(DateTime, default=datetime.utcnow)
    last_seen = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    is_active = Column(Boolean, default=True)

    # Relationships
    room = relationship("Room", back_populates="members")


class Operation(Base):
    """Operational Transform operation history"""

    __tablename__ = "operations"
    __table_args__ = (
        Index("idx_ops_room_version", "room_id", "version"),
        Index("idx_ops_room_timestamp", "room_id", "timestamp"),
    )

    id = Column(String(255), primary_key=True, index=True)
    room_id = Column(String(255), ForeignKey("rooms.id"), nullable=False, index=True)
    user_id = Column(String(255), nullable=False, index=True)
    operation_type = Column(String(50), nullable=False)  # insert, delete
    position = Column(Integer, nullable=False)
    content = Column(Text, nullable=False)
    version = Column(Integer, nullable=False, index=True)
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)
    meta_info = Column(JSON, nullable=True)

    # Relationships
    room = relationship("Room", back_populates="operations")

    def to_dict(self):
        return {
            "id": self.id,
            "room_id": self.room_id,
            "user_id": self.user_id,
            "type": self.operation_type,
            "position": self.position,
            "content": self.content,
            "version": self.version,
            "timestamp": self.timestamp.isoformat(),
        }


class Message(Base):
    """Chat message model"""

    __tablename__ = "messages"
    __table_args__ = (
        Index("idx_msgs_room_timestamp", "room_id", "timestamp"),
        Index("idx_msgs_user_timestamp", "user_id", "timestamp"),
    )

    id = Column(String(255), primary_key=True, index=True)
    room_id = Column(String(255), ForeignKey("rooms.id"), nullable=False, index=True)
    user_id = Column(String(255), nullable=False, index=True)
    username = Column(String(255), nullable=False)
    content = Column(Text, nullable=False)
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)
    edited_at = Column(DateTime, nullable=True)
    is_edited = Column(Boolean, default=False)
    reactions = Column(JSON, default={})  # {emoji: [user_ids]}
    is_deleted = Column(Boolean, default=False)

    # Relationships
    room = relationship("Room", back_populates="messages")

    def to_dict(self):
        return {
            "id": self.id,
            "room_id": self.room_id,
            "user_id": self.user_id,
            "username": self.username,
            "content": self.content,
            "timestamp": self.timestamp.isoformat(),
            "edited": self.is_edited,
            "editedAt": self.edited_at.isoformat() if self.edited_at else None,
            "reactions": self.reactions or {},
        }


class DocumentSnapshot(Base):
    """Periodic snapshots of document state (for performance)"""

    __tablename__ = "document_snapshots"

    id = Column(String(255), primary_key=True, index=True)
    room_id = Column(String(255), ForeignKey("rooms.id"), nullable=False, index=True)
    content = Column(Text, nullable=False)
    version = Column(Integer, nullable=False)
    size_bytes = Column(Integer, nullable=False)
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    def to_dict(self):
        return {
            "id": self.id,
            "room_id": self.room_id,
            "version": self.version,
            "size_bytes": self.size_bytes,
            "created_at": self.created_at.isoformat(),
        }


class ConflictResolution(Base):
    """Track conflict resolutions for analytics"""

    __tablename__ = "conflict_resolutions"

    id = Column(String(255), primary_key=True, index=True)
    room_id = Column(String(255), ForeignKey("rooms.id"), nullable=False, index=True)
    operation_id_1 = Column(String(255), nullable=False)
    operation_id_2 = Column(String(255), nullable=False)
    user_id_1 = Column(String(255), nullable=False)
    user_id_2 = Column(String(255), nullable=False)
    conflict_type = Column(
        String(50), nullable=False
    )  # insert-insert, delete-delete, etc
    resolution_strategy = Column(String(50), nullable=False)  # timestamp, user_id, etc
    resolved_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "id": self.id,
            "room_id": self.room_id,
            "operation_id_1": self.operation_id_1,
            "operation_id_2": self.operation_id_2,
            "conflict_type": self.conflict_type,
            "resolution_strategy": self.resolution_strategy,
            "resolved_at": self.resolved_at.isoformat(),
        }
