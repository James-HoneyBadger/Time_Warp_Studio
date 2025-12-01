"""
Collaboration client for Time Warp IDE.
Handles real-time collaborative coding features.
"""

import asyncio
import json
import logging
import threading
import time
from dataclasses import asdict, dataclass
from typing import Any, Callable, Dict, List, Optional, cast

logger = logging.getLogger(__name__)

try:
    import websockets

    WEBSOCKETS_AVAILABLE = True
except ImportError:
    WEBSOCKETS_AVAILABLE = False
    logger.warning(
        "websockets library not available: %s",
        "Install with: pip install websockets",
    )


@dataclass
class CollaborationUser:
    """User participating in collaboration session."""

    id: str
    name: str
    avatar: Optional[str] = None
    color: str = "#007acc"
    cursor_position: Optional[Dict[str, Any]] = None


@dataclass
class CollaborationOperation:
    """Operation for operational transform."""

    id: str
    user_id: str
    type: str  # 'insert', 'delete', 'replace'
    position: int
    content: str
    timestamp: float


class CollaborationClient:
    """Client for real-time collaborative coding.

    The client intentionally stores a number of runtime attributes (callbacks,
    internal state, thread handles) — allow the larger instance attribute
    count for this class.
    """

    # pylint: disable=too-many-instance-attributes

    def __init__(
        self,
        server_host: str = "localhost",
        server_port: int = 8765,
    ):
        self.server_host = server_host
        self.server_port = server_port
        self.websocket = None
        self.connected = False
        self.current_session_id = None
        self.user = None
        self.document_content = ""
        self.connected_users: Dict[str, CollaborationUser] = {}
        self.pending_operations: List[CollaborationOperation] = []
        self.lock = threading.Lock()

        # Callbacks
        self.on_connected: Optional[Callable] = None
        self.on_disconnected: Optional[Callable] = None
        self.on_operation_received: Optional[
            Callable[[CollaborationOperation], None]
        ] = None
        self.on_cursor_update: Optional[Callable[[str, Dict[str, Any]], None]] = None
        self.on_user_joined: Optional[Callable[[CollaborationUser], None]] = None
        self.on_user_left: Optional[Callable[[str], None]] = None
        self.on_session_joined: Optional[
            Callable[[str, str, List[CollaborationUser]], None]
        ] = None
        self.on_session_left: Optional[Callable[[str], None]] = None

        # Background thread for WebSocket communication
        self.ws_thread = None
        self.running = False

    def set_user(
        self,
        user_id: str,
        name: str,
        avatar: Optional[str] = None,
        color: str = "#007acc",
    ):
        """Set the current user."""
        self.user = CollaborationUser(
            id=user_id,
            name=name,
            avatar=avatar,
            color=color,
        )

    def connect(
        self,
        server: str = None,
        username: str = None,
        callback: Callable[[bool, str], None] = None,
    ) -> bool:
        """Connect to collaboration server."""
        if not WEBSOCKETS_AVAILABLE:
            logger.error("WebSocket library not available")
            if callback:
                callback(False, "WebSocket library not available")
            return False

        if self.connected:
            if callback:
                callback(True, "Already connected")
            return True

        # Parse server if provided
        if server:
            try:
                if ":" in server:
                    host, port_str = server.split(":", 1)
                    self.server_host = host
                    self.server_port = int(port_str)
                else:
                    self.server_host = server
                    self.server_port = 8765  # default port
            except ValueError:
                if callback:
                    callback(False, f"Invalid server format: {server}")
                return False

        # Set user if provided
        if username:
            self.set_user(f"user_{username}", username)

        self.running = True
        self.ws_thread = threading.Thread(target=self._run_websocket_client)
        self.ws_thread.daemon = True
        self.ws_thread.start()

        # Wait for connection
        timeout = 5
        start_time = time.time()
        while not self.connected and (time.time() - start_time) < timeout:
            time.sleep(0.1)

        success = self.connected
        message = "Connected successfully" if success else "Connection timeout"

        if callback:
            callback(success, message)

        return success

    def disconnect(self):
        """Disconnect from collaboration server."""
        self.running = False
        if self.ws_thread:
            self.ws_thread.join(timeout=2)

        self.connected = False
        self.current_session_id = None
        self.websocket = None

        if self.on_disconnected is not None:
            cb = cast(Callable[..., Any], self.on_disconnected)
            if callable(cb):
                cb()  # type: ignore

    def join_session(self, session_id: str) -> bool:
        """Join a collaboration session."""
        if not self.connected or not self.user:
            return False

        message = {
            "type": "join_session",
            "session_id": session_id,
            "user": asdict(self.user),
        }

        return self._send_message(message)

    def leave_session(self) -> bool:
        """Leave the current collaboration session."""
        if not self.connected or not self.current_session_id:
            return False

        message = {
            "type": "leave_session",
            "session_id": self.current_session_id,
            "user_id": self.user.id if self.user else "",
        }

        return self._send_message(message)

    def send_operation(self, operation: CollaborationOperation) -> bool:
        """Send an operation to the server."""
        if not self.connected or not self.current_session_id:
            return False

        message = {
            "type": "operation",
            "session_id": self.current_session_id,
            "operation": asdict(operation),
        }

        return self._send_message(message)

    def update_cursor(self, cursor_position: Dict[str, Any]) -> bool:
        """Update cursor position."""
        if not self.connected or not self.current_session_id or not self.user:
            return False

        message = {
            "type": "cursor_update",
            "session_id": self.current_session_id,
            "user_id": self.user.id,
            "cursor_position": cursor_position,
        }

        return self._send_message(message)

    def get_projects(self) -> bool:
        """Request list of available projects."""
        if not self.connected:
            return False

        message = {"type": "get_projects"}
        return self._send_message(message)

    def create_project(
        self,
        name: str,
        content: str = "",
        language: str = "basic",
        is_public: bool = False,
    ) -> bool:
        """Create a new project."""
        if not self.connected or not self.user:
            return False

        message = {
            "type": "create_project",
            "project": {
                "name": name,
                "owner_id": self.user.id,
                "content": content,
                "language": language,
                "is_public": is_public,
            },
        }

        return self._send_message(message)

    def join_project(self, project_id: str) -> bool:
        """Join a project session."""
        if not self.connected or not self.user:
            return False

        message = {
            "type": "join_project",
            "project_id": project_id,
            "user": asdict(self.user),
        }

        return self._send_message(message)

    def _run_websocket_client(self):
        """Run the WebSocket client in a background thread."""
        try:
            # Create event loop for this thread
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

            uri = f"ws://{self.server_host}:{self.server_port}"

            # Connect to server
            loop.run_until_complete(self._connect_websocket(uri))

        except Exception as e:  # pylint: disable=broad-except
            # Re-raise critical system signals to avoid hiding control events
            if isinstance(e, (KeyboardInterrupt, SystemExit)):
                raise
            logger.error("WebSocket client error: %s", e)
        finally:
            self.connected = False
            if self.on_disconnected is not None:
                cb = cast(Callable[..., Any], self.on_disconnected)
                if callable(cb):
                    cb()  # type: ignore

    async def _connect_websocket(self, uri: str):
        """Connect to WebSocket server."""
        try:
            async with websockets.connect(uri) as websocket:
                self.websocket = websocket
                self.connected = True

                logger.info("Connected to collaboration server at %s", uri)

                if self.on_connected is not None:
                    cb = cast(Callable[..., Any], self.on_connected)
                    if callable(cb):
                        cb()  # type: ignore

                # Message handling loop
                async for message in websocket:
                    try:
                        data = json.loads(message)
                        await self._handle_message(data)
                    except json.JSONDecodeError:
                        logger.warning("Invalid JSON message received")

        except websockets.exceptions.ConnectionClosed:
            logger.info("WebSocket connection closed")
        except Exception as e:  # pylint: disable=broad-except
            # Re-raise critical system signals to avoid hiding control events
            if isinstance(e, (KeyboardInterrupt, SystemExit)):
                raise
            logger.error("WebSocket connection error: %s", e)

    async def _handle_message(self, data: Dict[str, Any]):
        """Handle incoming message from server."""
        msg_type = data.get("type", "")

        if msg_type == "session_joined":  # type: ignore
            self._handle_session_joined(data)
        elif msg_type == "session_left":
            self._handle_session_left(data)
        elif msg_type == "operation_applied":
            self._handle_operation_applied(data)
        elif msg_type == "cursor_update":
            self._handle_cursor_update(data)
        elif msg_type == "user_joined":
            self._handle_user_joined(data)
        elif msg_type == "user_left":
            self._handle_user_left(data)
        elif msg_type == "projects_list":
            self._handle_projects_list(data)
        elif msg_type == "project_created":
            self._handle_project_created(data)
        elif msg_type == "error":
            logger.error(
                "Server error: %s",
                data.get("message", "Unknown error"),
            )
        else:
            logger.warning("Unknown message type: %s", msg_type)

    def _handle_session_joined(self, data: Dict[str, Any]):
        """Handle session joined message."""
        session_id = data.get("session_id")
        content = data.get("content", "")
        users_data = data.get("users", [])
        operations_data = data.get("operations", [])

        self.current_session_id = session_id
        self.document_content = content

        # Update connected users
        self.connected_users = {}
        for user_data in users_data:
            user = CollaborationUser(**user_data)
            self.connected_users[user.id] = user

        # Apply pending operations
        for op_data in operations_data:
            operation = CollaborationOperation(**op_data)
            self._apply_operation_to_document(operation)

        if self.on_session_joined is not None:
            users_list = list(self.connected_users.values())
            cb = cast(Callable[..., Any], self.on_session_joined)
            if callable(cb):
                cb(session_id, content, users_list)  # type: ignore

    def _handle_session_left(self, data: Dict[str, Any]):
        """Handle session left message."""
        session_id = data.get("session_id")

        if session_id == self.current_session_id:
            self.current_session_id = None
            self.connected_users.clear()

            if self.on_session_left is not None:
                cb = cast(Callable[..., Any], self.on_session_left)
                if callable(cb):
                    cb(session_id)  # type: ignore

    def _handle_operation_applied(self, data: Dict[str, Any]):
        """Handle operation applied message."""
        op_data = data.get("operation", {})
        operation = CollaborationOperation(**op_data)

        # Apply to local document
        self._apply_operation_to_document(operation)

        if self.on_operation_received is not None:
            cb = cast(Callable[..., Any], self.on_operation_received)
            if callable(cb):
                cb(operation)  # type: ignore

    def _handle_cursor_update(self, data: Dict[str, Any]):
        """Handle cursor update message."""
        user_id = data.get("user_id")
        cursor_position = data.get("cursor_position")

        if user_id and cursor_position and self.on_cursor_update is not None:
            # runtime-check the attribute and call via a cast so static
            # analyzers are satisfied consistently across the codebase.
            cb = cast(Callable[..., Any], self.on_cursor_update)
            if callable(cb):
                # Pylint may not infer the callable type here; runtime guard
                # ensures the call is safe.
                # pylint: disable=not-callable
                cb(user_id, cursor_position)  # type: ignore

    def _handle_user_joined(self, data: Dict[str, Any]):
        """Handle user joined message."""
        user_data = data.get("user", {})
        user = CollaborationUser(**user_data)

        with self.lock:
            self.connected_users[user.id] = user

        if self.on_user_joined is not None:
            cb = cast(Callable[..., Any], self.on_user_joined)
            if callable(cb):
                cb(user)  # type: ignore

    def _handle_user_left(self, data: Dict[str, Any]):
        """Handle user left message."""
        user_id = data.get("user_id")

        with self.lock:
            self.connected_users.pop(user_id, None)

        if self.on_user_left is not None:
            cb = cast(Callable[..., Any], self.on_user_left)
            if callable(cb):
                cb(user_id)  # type: ignore

    def _handle_projects_list(self, data: Dict[str, Any]):
        """Handle projects list message."""
        # This would typically trigger a callback to update UI
        projects = data.get("projects", [])
        logger.info("Received %s projects", len(projects))

    def _handle_project_created(self, data: Dict[str, Any]):
        """Handle project created message."""
        project = data.get("project", {})
        logger.info("Project created: %s", project.get("name", "Unknown"))

    def _apply_operation_to_document(self, operation: CollaborationOperation):
        """Apply an operation to the local document."""
        with self.lock:
            if operation.type == "insert":
                self.document_content = (
                    self.document_content[: operation.position]
                    + operation.content  # type: ignore
                    + self.document_content[operation.position :]
                )
            elif operation.type == "delete":
                end_pos = operation.position + len(operation.content)
                self.document_content = (
                    self.document_content[: operation.position]
                    + self.document_content[end_pos:]
                )
            elif operation.type == "replace":
                end_pos = operation.position + len(operation.content)
                self.document_content = (
                    self.document_content[: operation.position]
                    + operation.content
                    + self.document_content[end_pos:]
                )

    def _send_message(self, message: Dict[str, Any]) -> bool:
        """Send a message to the server."""
        if not self.websocket or not self.connected:
            return False

        try:
            # Send in the WebSocket thread's event loop
            asyncio.create_task(self.websocket.send(json.dumps(message)))
            return True
        except (OSError, ValueError) as e:
            logger.error("Failed to send message: %s", e)
            return False

    def get_connected_users(self) -> List[CollaborationUser]:
        """Get list of connected users."""
        with self.lock:
            return list(self.connected_users.values())

    def get_document_content(self) -> str:
        """Get current document content."""
        with self.lock:
            return self.document_content

    def is_connected(self) -> bool:
        """Check if connected to server."""
        return self.connected

    def get_current_session_id(self) -> Optional[str]:
        """Get current session ID."""
        return self.current_session_id
