"""
REST API Routes for Room Management
Handles HTTP endpoints for room operations and synchronization
"""

import logging
from datetime import datetime
from typing import List

from fastapi import APIRouter, Depends, HTTPException, status
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from ..db import get_session
from ..services import ChatService, RoomService, SyncService

logger = logging.getLogger(__name__)
router = APIRouter(prefix="/api/rooms", tags=["rooms"])


# Pydantic schemas for validation


class RoomCreate(BaseModel):
    """Schema for room creation"""

    name: str
    is_private: bool = False


class RoomResponse(BaseModel):
    """Schema for room response"""

    id: str
    name: str
    owner_id: str
    is_private: bool
    created_at: datetime

    class Config:
        from_attributes = True


class MemberResponse(BaseModel):
    """Schema for room member response"""

    id: str
    user_id: str
    user_name: str
    role: str
    joined_at: datetime

    class Config:
        from_attributes = True


class OperationResponse(BaseModel):
    """Schema for operation response"""

    id: str
    user_id: str
    type: str
    position: int
    content: str
    version: int
    timestamp: datetime

    class Config:
        from_attributes = True


class MessageResponse(BaseModel):
    """Schema for message response"""

    id: str
    user_id: str
    username: str
    content: str
    timestamp: datetime

    class Config:
        from_attributes = True


# Routes


@router.post("", response_model=RoomResponse)
async def create_room(
    room_data: RoomCreate,
    owner_id: str,
    session: AsyncSession = Depends(get_session),
):
    """Create a new collaboration room"""
    try:
        service = RoomService(session)
        room = await service.create_room(room_data.name, owner_id, room_data.is_private)
        return room
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error creating room: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to create room",
        )


@router.get("/{room_id}", response_model=RoomResponse)
async def get_room(room_id: str, session: AsyncSession = Depends(get_session)):
    """Get room details"""
    try:
        service = RoomService(session)
        room = await service.get_room(room_id)
        if not room:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND, detail="Room not found"
            )
        return room
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting room: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get room",
        )


@router.get("/{room_id}/members", response_model=List[MemberResponse])
async def get_room_members(room_id: str, session: AsyncSession = Depends(get_session)):
    """Get all members in a room"""
    try:
        service = RoomService(session)
        members = await service.get_room_members(room_id)
        return members
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting room members: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get members",
        )


@router.post("/{room_id}/members")
async def add_room_member(
    room_id: str,
    user_id: str,
    user_name: str,
    user_email: str | None = None,
    session: AsyncSession = Depends(get_session),
):
    """Add member to room"""
    try:
        service = RoomService(session)
        member = await service.add_member(room_id, user_id, user_name, user_email)
        return {"message": "Member added", "member_id": member.id}
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error adding member: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to add member",
        )


@router.delete("/{room_id}/members/{user_id}")
async def remove_room_member(
    room_id: str, user_id: str, session: AsyncSession = Depends(get_session)
):
    """Remove member from room"""
    try:
        service = RoomService(session)
        success = await service.remove_member(room_id, user_id)
        if not success:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Member not found",
            )
        return {"message": "Member removed"}
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error removing member: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to remove member",
        )


@router.get("/{room_id}/operations", response_model=List[OperationResponse])
async def get_room_operations(
    room_id: str,
    limit: int = 100,
    session: AsyncSession = Depends(get_session),
):
    """Get operation history for room"""
    try:
        service = SyncService(session)
        operations = await service.op_repo.get_room_operations(room_id, limit)
        return operations
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting operations: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get operations",
        )


@router.get("/{room_id}/operations/since/{version}")
async def get_operations_since(
    room_id: str, version: int, session: AsyncSession = Depends(get_session)
):
    """Get operations after a specific version"""
    try:
        service = SyncService(session)
        operations = await service.get_operations_since(room_id, version)
        return {
            "version": await service.get_latest_version(room_id),
            "operations": [op.to_dict() for op in operations],
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting operations: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get operations",
        )


@router.get("/{room_id}/messages", response_model=List[MessageResponse])
async def get_room_messages(
    room_id: str,
    limit: int = 50,
    offset: int = 0,
    session: AsyncSession = Depends(get_session),
):
    """Get chat messages from room"""
    try:
        service = ChatService(session)
        messages = await service.get_room_messages(room_id, limit, offset)
        return messages
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting messages: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get messages",
        )


@router.get("/{room_id}/messages/search")
async def search_messages(
    room_id: str, query: str, session: AsyncSession = Depends(get_session)
):
    """Search messages in room"""
    try:
        service = ChatService(session)
        messages = await service.search_messages(room_id, query)
        return {"query": query, "results": [msg.to_dict() for msg in messages]}
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error searching messages: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to search messages",
        )


@router.delete("/{room_id}")
async def delete_room(room_id: str, session: AsyncSession = Depends(get_session)):
    """Delete a room"""
    try:
        service = RoomService(session)
        success = await service.delete_room(room_id)
        if not success:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND, detail="Room not found"
            )
        return {"message": "Room deleted"}
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error deleting room: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to delete room",
        )
