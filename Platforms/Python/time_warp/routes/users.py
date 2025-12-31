"""
REST API Routes for User Management
Handles HTTP endpoints for user operations and profile management
"""

import logging
from typing import List

from fastapi import APIRouter, Depends, HTTPException, status
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from ..db import get_session
from ..services import RoomService

logger = logging.getLogger(__name__)
router = APIRouter(prefix="/api/users", tags=["users"])


class UserProfile(BaseModel):
    """Schema for user profile"""

    id: str
    email: str
    name: str
    avatar_url: str = None
    status: str = "online"


class UserRoomsResponse(BaseModel):
    """Schema for user's rooms"""

    id: str
    name: str
    owner_id: str
    is_private: bool
    member_count: int


@router.get("/{user_id}/profile", response_model=UserProfile)
async def get_user_profile(user_id: str):
    """Get user profile (from auth service)"""
    # This would normally fetch from user service
    return {
        "id": user_id,
        "email": f"user_{user_id}@example.com",
        "name": f"User {user_id}",
        "status": "online",
    }


@router.get("/{user_id}/rooms", response_model=List[UserRoomsResponse])
async def get_user_rooms(
    user_id: str, session: AsyncSession = Depends(get_session)
):
    """Get all rooms for a user"""
    try:
        service = RoomService(session)
        rooms = await service.get_user_rooms(user_id)
        return [
            {
                "id": room.id,
                "name": room.name,
                "owner_id": room.owner_id,
                "is_private": room.is_private,
                "member_count": (
                    len(room.members) if hasattr(room, "members") else 0
                ),
            }
            for room in rooms
        ]
    except Exception as e:
        logger.error(f"Error getting user rooms: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get user rooms",
        )


@router.post("/{user_id}/rooms/{room_id}/join")
async def join_room(
    user_id: str,
    room_id: str,
    username: str,
    session: AsyncSession = Depends(get_session),
):
    """User joins a room"""
    try:
        service = RoomService(session)
        member = await service.add_member(room_id, user_id, username)
        return {"message": "Joined room", "member_id": member.id}
    except Exception as e:
        logger.error(f"Error joining room: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to join room",
        )


@router.post("/{user_id}/rooms/{room_id}/leave")
async def leave_room(
    user_id: str, room_id: str, session: AsyncSession = Depends(get_session)
):
    """User leaves a room"""
    try:
        service = RoomService(session)
        success = await service.remove_member(room_id, user_id)
        if not success:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail="Member not found",
            )
        return {"message": "Left room"}
    except Exception as e:
        logger.error(f"Error leaving room: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to leave room",
        )


@router.get("/{user_id}/status")
async def get_user_status(user_id: str):
    """Get user status"""
    # This would normally fetch from user service
    return {
        "user_id": user_id,
        "status": "online",
        "last_seen": "2025-12-31T00:00:00Z",
    }


@router.put("/{user_id}/status")
async def update_user_status(user_id: str, status: str):
    """Update user status (online/away/offline)"""
    if status not in ["online", "away", "offline"]:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid status"
        )
    return {"user_id": user_id, "status": status}
