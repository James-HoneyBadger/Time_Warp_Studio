"""
WebSocket Routes for Real-time Collaboration
Handles all WebSocket connections and message routing
"""

import json
import logging
import uuid
from typing import Dict

from fastapi import APIRouter, Depends, WebSocket, WebSocketDisconnect

from ..core.chat_service import ChatService
from ..core.collaboration_engine import OperationalTransform
from ..core.presence_service import PresenceService
from ..core.websocket_manager import ConnectionManager

logger = logging.getLogger(__name__)

router = APIRouter()

# Singleton instances (would be injected in production)
connection_manager = ConnectionManager()
presence_service = PresenceService()
chat_service = ChatService()

# Room-based OT engines: {room_id: OT instance}
ot_engines: Dict[str, OperationalTransform] = {}


def get_ot_engine(room_id: str) -> OperationalTransform:
    """Get or create OT engine for room"""
    if room_id not in ot_engines:
        ot_engines[room_id] = OperationalTransform()
    return ot_engines[room_id]


@router.websocket("/ws/{room_id}/{user_id}")
async def websocket_endpoint(websocket: WebSocket, room_id: str, user_id: str):
    """
    Main WebSocket endpoint for real-time collaboration
    URL: /ws/{room_id}/{user_id}
    """
    connection_id = str(uuid.uuid4())

    try:
        # Receive initial user data
        data = await websocket.receive_json()
        user_data = {
            "id": user_id,
            "name": data.get("name", "Anonymous"),
            "email": data.get("email", ""),
            "color": data.get("color", "#3B82F6"),
        }

        # Accept connection
        await connection_manager.connect(connection_id, websocket, user_data)
        logger.info(f"User {user_data['name']} connected: {connection_id}")

        # Join room
        await connection_manager.join_room(connection_id, room_id)

        # Set presence
        presence_service.set_user_presence(connection_id, room_id, user_data)

        # Notify others
        await connection_manager.broadcast_to_room(
            room_id,
            {
                "type": "user_joined",
                "userId": user_id,
                "name": user_data["name"],
                "color": user_data["color"],
            },
            exclude_connection=connection_id,
        )

        # Main message loop
        while True:
            data = await websocket.receive_json()
            message_type = data.get("type")

            # Code change event
            if message_type == "code_change":
                await handle_code_change(
                    room_id,
                    connection_id,
                    user_id,
                    data,
                    connection_manager,
                    ot_engines,
                )

            # Cursor update event
            elif message_type == "cursor_update":
                position = data.get("position", {"line": 0, "column": 0})
                update = presence_service.update_cursor_position(
                    connection_id, room_id, position
                )
                await connection_manager.broadcast_to_room(
                    room_id,
                    update,
                    exclude_connection=connection_id,
                )

            # Presence update event
            elif message_type == "presence_update":
                status = data.get("status", "idle")
                update = presence_service.update_user_status(
                    connection_id, room_id, status
                )
                await connection_manager.broadcast_to_room(
                    room_id,
                    update,
                    exclude_connection=connection_id,
                )

            # Typing event
            elif message_type == "typing":
                is_typing = data.get("isTyping", False)
                update = presence_service.set_typing(
                    connection_id, room_id, is_typing
                )
                await connection_manager.broadcast_to_room(
                    room_id,
                    update,
                    exclude_connection=connection_id,
                )

            # Chat message event
            elif message_type == "chat_message":
                message = chat_service.add_message(
                    room_id,
                    user_id,
                    user_data["name"],
                    data.get("content", ""),
                )
                await connection_manager.broadcast_to_room(
                    room_id,
                    {
                        "type": "chat_message",
                        "id": message.id,
                        "userId": message.user_id,
                        "username": message.username,
                        "content": message.content,
                        "timestamp": message.timestamp,
                    },
                )

            # Message reaction event
            elif message_type == "add_reaction":
                message_id = data.get("messageId")
                emoji = data.get("emoji")
                chat_service.add_reaction(room_id, message_id, emoji, user_id)
                await connection_manager.broadcast_to_room(
                    room_id,
                    {
                        "type": "reaction_added",
                        "messageId": message_id,
                        "emoji": emoji,
                        "userId": user_id,
                    },
                )

            # Sync request event
            elif message_type == "sync_request":
                version = data.get("version", 0)
                ot = get_ot_engine(room_id)
                operations = ot.get_operations_since(version)
                await connection_manager.send_to_connection(
                    connection_id,
                    {
                        "type": "sync_response",
                        "version": ot.version,
                        "operations": [op.to_dict() for op in operations],
                        "content": ot.content,
                    },
                )

            # Get room state event
            elif message_type == "get_room_state":
                ot = get_ot_engine(room_id)
                users = connection_manager.get_room_users(room_id)
                messages = chat_service.get_room_messages(room_id, limit=50)

                await connection_manager.send_to_connection(
                    connection_id,
                    {
                        "type": "room_state",
                        "roomId": room_id,
                        "content": ot.content,
                        "version": ot.version,
                        "users": users,
                        "messages": messages,
                        "presence": presence_service.get_room_presence(
                            room_id
                        ),
                    },
                )

            else:
                logger.warning(f"Unknown message type: {message_type}")

    except WebSocketDisconnect:
        logger.info(f"User disconnected: {connection_id}")
        await handle_disconnect(
            connection_id, room_id, connection_manager, presence_service
        )

    except Exception as e:
        logger.error(f"WebSocket error: {e}")
        await handle_disconnect(
            connection_id, room_id, connection_manager, presence_service
        )
        raise


async def handle_code_change(
    room_id, connection_id, user_id, data, manager, engines
):
    """Handle code change with Operational Transform"""
    ot = get_ot_engine(room_id)

    op_type = data.get("type", "insert")
    position = data.get("position", 0)
    content = data.get("content", "")

    # Create operation
    operation = ot.create_operation(user_id, op_type, position, content)

    # Get pending operations from other users
    pending = [
        op
        for op in ot.operation_history
        if op.user_id != user_id and op.version >= operation.version - 10
    ]

    # Apply with transformation
    success, message = ot.apply_operation(operation, transform_against=pending)

    if success:
        # Broadcast change to all users
        await manager.broadcast_to_room(
            room_id,
            {
                "type": "code_change",
                "id": operation.id,
                "userId": user_id,
                "operationType": op_type,
                "position": operation.position,
                "content": operation.content,
                "version": ot.version,
                "timestamp": operation.timestamp,
            },
        )
        logger.info(f"Code change applied in room {room_id}: {op_type}")
    else:
        await manager.send_to_connection(
            connection_id,
            {"type": "error", "message": message},
        )


async def handle_disconnect(connection_id, room_id, manager, presence_service):
    """Handle user disconnect"""
    # Remove from presence
    presence_service.remove_user_presence(connection_id, room_id)

    # Remove from room
    await manager.leave_room(connection_id, room_id)

    # Remove connection
    manager.disconnect(connection_id)

    logger.info(f"User {connection_id} disconnected from room {room_id}")
