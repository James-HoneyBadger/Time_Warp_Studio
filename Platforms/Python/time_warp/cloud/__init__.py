"""time_warp.cloud — DISABLED stub

This module used to provide a full WebSocket-based collaboration server.
In the current build collaboration is disabled. The module keeps a
minimal API-compatible surface so imports continue to work; the
implementation returns safe disabled responses.
"""

import sys
from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass
class User:  # compatibility dataclass
    id: str
    name: str
    avatar: Optional[str] = None


@dataclass
class Project:  # compatibility dataclass
    id: str
    name: str
    owner_id: str
    language: str
    content: str


@dataclass
class Operation:  # compatibility dataclass
    id: str
    user_id: str
    type: str
    position: int
    content: str = ""


class OperationalTransform:  # compatibility placeholder
    """Compatibility placeholder for an operational transform helper.

    This no-op implementation is kept so callers can import and call
    the transform operation API even when collaboration is disabled.
    """

    @staticmethod
    def transform_operation(
        operation: Operation, _concurrent_ops: List[Operation]
    ) -> Operation:
        """Return the operation unchanged (disabled collaboration).

        The `_concurrent_ops` parameter is intentionally unused in the
        disabled stub.
        """
        return operation


class CollaborationSession:  # compatibility placeholder
    """Compatibility placeholder for a collaboration session.

    In the real distribution Sessions would apply operational transforms
    and manage user cursors/content. Here we only store a simple document
    content string for tests and compatibility.
    """

    def __init__(self, project_id: str, owner_id: str):
        self.project_id = project_id
        self.owner_id = owner_id
        self.document_content = ""

    def get_document_content(self) -> str:
        """Return the current document contents for the session."""
        return self.document_content

    def set_document_content(self, content: str):
        """Replace the session document contents.

        This stub does not perform operational transforms.
        """
        self.document_content = content


class CloudCollaborationServer:
    """Disabled collaboration server — API-compatible no-op server.

    The server exposes a small compatibility surface so callers can
    check session and server information without enabling networking.
    """

    def __init__(self, host: str = "localhost", port: int = 8765):
        self.host = host
        self.port = port
        self.sessions: Dict[str, CollaborationSession] = {}

    def start(self) -> bool:
        # Collaboration disabled
        return False

    def stop(self) -> None:
        return None

    # Minimal introspection helpers kept for compatibility
    def get_session_info(self, session_id: str) -> Optional[Dict[str, Any]]:
        session = self.sessions.get(session_id)
        if not session:
            return None
        return {
            "session_id": session_id,
            "content_length": len(session.get_document_content()),
        }

    def get_server_stats(self) -> Dict[str, Any]:
        """Return minimal, zeroed server statistics in disabled mode.

        The keys mirror historical server statistics so callers don't
        break when collaboration is disabled.
        """

        return {
            "active_sessions": 0,
            "connected_clients": 0,
            "total_projects": 0,
            "uptime": 0,
        }


# Global server instance (compat)
_SERVER_INSTANCE = None


def get_collaboration_server() -> CloudCollaborationServer:
    """Return a singleton CloudCollaborationServer instance.

    The instance is lazily created and returned for backwards
    compatibility with older code paths.
    """
    # Avoid using a module-level global statement; access the module
    # object and store the singleton there. This keeps pylint happy.
    mod = sys.modules[__name__]
    inst = getattr(mod, "_SERVER_INSTANCE", None)
    if inst is None:
        inst = CloudCollaborationServer()
        setattr(mod, "_SERVER_INSTANCE", inst)
    return inst
