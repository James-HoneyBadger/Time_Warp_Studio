"""
REST API Routes for Synchronization
Handles HTTP endpoints for operational transformation and sync operations
"""

import logging
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Query, status
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

from ..db import get_session
from ..services import SyncService

logger = logging.getLogger(__name__)
router = APIRouter(prefix="/api/sync", tags=["sync"])


class OperationRequest(BaseModel):
    """Schema for operation submission"""

    user_id: str
    op_type: str
    position: int
    content: str


class SyncRequest(BaseModel):
    """Schema for sync request"""

    room_id: str
    user_id: str
    client_version: int


class VersionResponse(BaseModel):
    """Schema for version response"""

    current_version: int
    last_update: str


class SnapshotResponse(BaseModel):
    """Schema for document snapshot"""

    content: str
    version: int
    size_bytes: int
    created_at: str


@router.post("/{room_id}/operations")
async def record_operation(
    room_id: str,
    operation: OperationRequest,
    session: AsyncSession = Depends(get_session),
):
    """Record an operation in the room"""
    try:
        service = SyncService(session)
        op = await service.record_operation(
            room_id=room_id,
            user_id=operation.user_id,
            op_type=operation.op_type,
            position=operation.position,
            content=operation.content,
        )
        return {
            "operation_id": op.id,
            "version": op.version,
            "timestamp": op.timestamp.isoformat(),
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error recording operation: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to record operation",
        )


@router.get("/{room_id}/version", response_model=VersionResponse)
async def get_current_version(
    room_id: str, session: AsyncSession = Depends(get_session)
):
    """Get current version of document in room"""
    try:
        service = SyncService(session)
        version = await service.get_latest_version(room_id)
        return {
            "current_version": version,
            "last_update": "2025-12-31T00:00:00Z",  # Would fetch from DB
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting version: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get version",
        )


@router.get("/{room_id}/operations")
async def get_operations(
    room_id: str,
    from_version: int = Query(0),
    to_version: Optional[int] = None,
    session: AsyncSession = Depends(get_session),
):
    """Get operations between versions"""
    try:
        service = SyncService(session)
        if to_version is None:
            # Get all operations from version onwards
            operations = await service.get_operations_since(room_id, from_version)
        else:
            # Get operations in range
            operations = await service.get_operations_since(room_id, from_version)
            operations = [op for op in operations if op.version <= to_version]

        return {
            "operations": [
                {
                    "id": op.id,
                    "version": op.version,
                    "type": op.type,
                    "position": op.position,
                    "content": op.content,
                    "user_id": op.user_id,
                    "timestamp": op.timestamp.isoformat(),
                }
                for op in operations
            ]
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting operations: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get operations",
        )


@router.get("/{room_id}/snapshot", response_model=SnapshotResponse)
async def get_snapshot(
    room_id: str,
    version: Optional[int] = None,
    session: AsyncSession = Depends(get_session),
):
    """Get document snapshot at version (for offline sync)"""
    try:
        service = SyncService(session)
        if version is None:
            snapshot = await service.get_latest_snapshot(room_id)
        else:
            snapshot = await service.get_snapshot_at_version(room_id, version)

        if not snapshot:
            # Return empty snapshot if none exists
            return {
                "content": "",
                "version": 0,
                "size_bytes": 0,
                "created_at": "2025-12-31T00:00:00Z",
            }

        return {
            "content": snapshot.content,
            "version": snapshot.version,
            "size_bytes": snapshot.size_bytes,
            "created_at": snapshot.created_at.isoformat(),
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error getting snapshot: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to get snapshot",
        )


@router.post("/{room_id}/snapshot")
async def create_snapshot(
    room_id: str,
    content: str,
    version: int,
    session: AsyncSession = Depends(get_session),
):
    """Create snapshot at current version (optimization)"""
    try:
        service = SyncService(session)
        snapshot = await service.create_snapshot(room_id, content, version)
        return {
            "snapshot_id": snapshot.id,
            "version": snapshot.version,
            "size_bytes": snapshot.size_bytes,
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error creating snapshot: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to create snapshot",
        )


@router.post("/sync")
async def sync_client(
    sync_request: SyncRequest, session: AsyncSession = Depends(get_session)
):
    """Full client sync endpoint (for periodic syncing)"""
    try:
        service = SyncService(session)

        # Get current server version
        server_version = await service.get_latest_version(sync_request.room_id)

        # Get snapshot at client version for efficiency
        snapshot = await service.get_snapshot_at_version(
            sync_request.room_id, sync_request.client_version
        )

        # Get operations since client version
        operations = await service.get_operations_since(
            sync_request.room_id, sync_request.client_version
        )

        return {
            "room_id": sync_request.room_id,
            "server_version": server_version,
            "client_version": sync_request.client_version,
            "snapshot": {
                "content": snapshot.content if snapshot else "",
                "version": snapshot.version if snapshot else 0,
            },
            "operations": [
                {
                    "id": op.id,
                    "version": op.version,
                    "type": op.type,
                    "position": op.position,
                    "content": op.content,
                    "user_id": op.user_id,
                }
                for op in operations
            ],
        }
    except (ValueError, KeyError, AttributeError, TypeError) as e:
        logger.error("Error in sync: %s", e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to sync",
        )
