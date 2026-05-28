"""Local collaborative programming sessions (LAN pair programming)."""

import json
import logging
import socket
import struct
import threading
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Callable, Dict, List, Optional, Set, Tuple

logger = logging.getLogger(__name__)


class SessionState(Enum):
    """States of a collaborative session."""

    IDLE = "idle"
    DISCOVERING = "discovering"
    CONNECTING = "connecting"
    CONNECTED = "connected"
    SYNCING = "syncing"
    DISCONNECTED = "disconnected"
    ERROR = "error"


class MessageType(Enum):
    """Types of collaboration messages."""

    PING = "ping"
    DISCOVER = "discover"
    CONNECT = "connect"
    DISCONNECT = "disconnect"
    CODE_UPDATE = "code_update"
    CURSOR_MOVE = "cursor_move"
    EXECUTE_CODE = "execute_code"
    OUTPUT = "output"
    CHAT = "chat"
    SYNC_REQUEST = "sync_request"
    SYNC_RESPONSE = "sync_response"


@dataclass
class Participant:
    """Represents a session participant."""

    user_id: str
    username: str
    color: str  # Color for cursor/selection highlighting
    ip_address: str
    port: int
    is_host: bool = False
    last_seen: float = field(default_factory=time.time)
    cursor_line: int = 0
    cursor_col: int = 0


@dataclass
class CodeChange:
    """Represents a code change."""

    user_id: str
    timestamp: float
    line: int
    old_text: str
    new_text: str
    change_type: str  # 'insert', 'delete', 'replace'


@dataclass
class CollaborativeMessage:
    """Message in collaborative session."""

    msg_type: MessageType
    sender_id: str
    timestamp: float
    data: Dict = field(default_factory=dict)


class LocalCollaborationSession:
    """Manages local LAN-based pair programming sessions."""

    TCP_PORT = 54321  # TCP port for peer-to-peer collaboration

    def __init__(self, username: str, user_id: Optional[str] = None):
        """Initialize collaboration session."""
        self.username = username
        self.user_id = user_id or self._generate_user_id()
        self.is_host = False
        self.state = SessionState.IDLE
        self.participants: Dict[str, Participant] = {}
        self.code_changes: List[CodeChange] = []
        self.shared_code = ""
        self.message_history: List[CollaborativeMessage] = []

        self._callbacks: Dict[str, List[Callable]] = {}
        self._discovery_active = False
        self._socket: Optional[socket.socket] = None
        self._peers: Set[str] = set()

        # TCP state
        self._lock = threading.Lock()
        self._peer_socks: List[socket.socket] = []
        self._server_sock: Optional[socket.socket] = None
        self._recv_threads: List[threading.Thread] = []

    def _generate_user_id(self) -> str:
        """Generate unique user ID."""
        import uuid

        return str(uuid.uuid4())[:8]

    def start_session(self, code: str = "") -> bool:
        """
        Start a new collaborative session.

        Returns: True if successful
        """
        try:
            self.is_host = True
            self.state = SessionState.CONNECTING
            self.shared_code = code

            # Add self as participant
            self.participants[self.user_id] = Participant(
                user_id=self.user_id,
                username=self.username,
                color="#FF6B6B",
                ip_address="127.0.0.1",
                port=0,
                is_host=True,
            )

            self.state = SessionState.CONNECTED
            self._start_tcp_server()
            self._trigger_event("session_started")
            return True

        except Exception as e:
            self.state = SessionState.ERROR
            self._trigger_event("error", str(e))
            return False

    def discover_sessions(self, timeout: int = 5) -> List[Dict]:
        """
        Discover collaborative sessions on LAN.

        Returns: List of available session info
        """
        self.state = SessionState.DISCOVERING
        self._discovery_active = True
        discovered = []

        try:
            # Broadcast discovery message
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
            sock.settimeout(timeout)

            message = {
                "type": "discover",
                "user_id": self.user_id,
                "username": self.username,
                "timestamp": time.time(),
            }

            # Try common ports
            for port in [9000, 9001, 9002, 9003]:
                try:
                    sock.sendto(json.dumps(message).encode(), ("<broadcast>", port))
                except BaseException:
                    pass

            # Listen for responses
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            sock.bind(("", 9000))
            sock.settimeout(timeout)

            while self._discovery_active:
                try:
                    data, addr = sock.recvfrom(1024)
                    response = json.loads(data.decode())
                    if response.get("type") == "discover_response":
                        discovered.append(
                            {
                                "user_id": response["user_id"],
                                "username": response["username"],
                                "ip": addr[0],
                                "port": response.get("port", 9000),
                            }
                        )
                except socket.timeout:
                    break

            sock.close()

        except Exception as e:
            self._trigger_event("error", f"Discovery failed: {e}")

        self.state = SessionState.IDLE
        return discovered

    def join_session(self, host_user_id: str, host_ip: str, host_port: int) -> bool:
        """
        Join an existing collaborative session.

        Returns: True if successful
        """
        try:
            self.state = SessionState.CONNECTING

            # Create peer entry
            self.participants[host_user_id] = Participant(
                user_id=host_user_id,
                username=f"Host-{host_user_id[:4]}",
                color="#4ECDC4",
                ip_address=host_ip,
                port=host_port,
                is_host=True,
            )

            # Add self
            self.participants[self.user_id] = Participant(
                user_id=self.user_id,
                username=self.username,
                color="#95E1D3",
                ip_address=socket.gethostbyname(socket.gethostname()),
                port=9000,
                is_host=False,
            )

            self.state = SessionState.CONNECTED
            self._trigger_event("session_joined")
            self._start_tcp_client(host_ip, self.TCP_PORT)

            # Request full code sync
            self._send_message(MessageType.SYNC_REQUEST, {"user_id": self.user_id})

            return True

        except Exception as e:
            self.state = SessionState.ERROR
            self._trigger_event("error", str(e))
            return False

    def update_code(self, line: int, old_text: str, new_text: str):
        """Record a code change."""
        change = CodeChange(
            user_id=self.user_id,
            timestamp=time.time(),
            line=line,
            old_text=old_text,
            new_text=new_text,
            change_type="replace",
        )

        self.code_changes.append(change)

        # Broadcast change to peers
        self._send_message(
            MessageType.CODE_UPDATE,
            {
                "line": line,
                "old_text": old_text,
                "new_text": new_text,
                "timestamp": change.timestamp,
            },
        )

        self._trigger_event("code_changed", change)

    def update_cursor(self, line: int, col: int):
        """Update cursor position and broadcast to peers."""
        if self.user_id in self.participants:
            self.participants[self.user_id].cursor_line = line
            self.participants[self.user_id].cursor_col = col

        self._send_message(MessageType.CURSOR_MOVE, {"line": line, "col": col})

        self._trigger_event("cursor_moved", {"line": line, "col": col})

    def broadcast_output(self, output: str):
        """Broadcast program output to all participants."""
        self._send_message(MessageType.OUTPUT, {"content": output})

        self._trigger_event("output_received", output)

    def send_chat(self, message: str):
        """Send chat message to all participants."""
        chat_msg = CollaborativeMessage(
            msg_type=MessageType.CHAT,
            sender_id=self.user_id,
            timestamp=time.time(),
            data={"content": message},
        )

        self.message_history.append(chat_msg)
        self._send_message(MessageType.CHAT, {"content": message})
        self._trigger_event("chat_message", message)

    def get_participants(self) -> List[Participant]:
        """Get list of session participants."""
        return list(self.participants.values())

    def get_cursor_positions(self) -> Dict[str, Tuple[int, int]]:
        """Get cursor positions of all participants."""
        return {
            p.user_id: (p.cursor_line, p.cursor_col) for p in self.participants.values()
        }

    def get_participant_color(self, user_id: str) -> str:
        """Get display color for a participant."""
        if user_id in self.participants:
            return self.participants[user_id].color
        return "#999999"

    def disconnect(self):
        """Disconnect from session."""
        try:
            self._send_message(MessageType.DISCONNECT, {"user_id": self.user_id})
        except BaseException:
            pass

        self.state = SessionState.DISCONNECTED
        self.participants.clear()

        # Close all peer sockets
        with self._lock:
            for sock in self._peer_socks:
                try:
                    sock.close()
                except OSError:
                    pass
            self._peer_socks.clear()

        # Close server socket
        if self._server_sock is not None:
            try:
                self._server_sock.close()
            except OSError:
                pass
            self._server_sock = None

        self._trigger_event("disconnected")

    def export_session_log(self) -> Dict:
        """Export session history as JSON."""
        return {
            "user_id": self.user_id,
            "username": self.username,
            "is_host": self.is_host,
            "participants": [
                {
                    "user_id": p.user_id,
                    "username": p.username,
                    "is_host": p.is_host,
                }
                for p in self.participants.values()
            ],
            "changes_count": len(self.code_changes),
            "messages_count": len(self.message_history),
            "duration": self._calculate_session_duration(),
        }

    def _calculate_session_duration(self) -> float:
        """Calculate session duration in seconds."""
        if not self.code_changes and not self.message_history:
            return 0

        times = [c.timestamp for c in self.code_changes]
        times.extend([m.timestamp for m in self.message_history])

        if times:
            return max(times) - min(times)
        return 0

    def _send_message(self, msg_type: MessageType, data: Dict):
        """Send message to connected peers and record locally."""
        message = CollaborativeMessage(
            msg_type=msg_type,
            sender_id=self.user_id,
            timestamp=time.time(),
            data=data,
        )
        self.message_history.append(message)

        # Encode as 4-byte big-endian length prefix + UTF-8 JSON body
        payload = json.dumps(
            {
                "type": msg_type.value,
                "sender_id": self.user_id,
                "timestamp": message.timestamp,
                "data": data,
            }
        ).encode("utf-8")
        frame = struct.pack(">I", len(payload)) + payload

        with self._lock:
            dead: List[socket.socket] = []
            for sock in self._peer_socks:
                try:
                    sock.sendall(frame)
                except OSError:
                    dead.append(sock)
            for sock in dead:
                self._peer_socks.remove(sock)
                try:
                    sock.close()
                except OSError:
                    pass

    # ------------------------------------------------------------------
    # TCP server / client helpers
    # ------------------------------------------------------------------

    def _start_tcp_server(self) -> None:
        """Bind TCP server socket and start the accept loop (host side)."""
        try:
            srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            srv.bind(("", self.TCP_PORT))
            srv.listen(8)
            self._server_sock = srv
            t = threading.Thread(target=self._accept_loop, daemon=True)
            t.start()
            self._recv_threads.append(t)
            logger.info("Collaboration TCP server listening on port %d", self.TCP_PORT)
        except OSError as exc:
            logger.warning("Cannot start TCP collaboration server: %s", exc)

    def _start_tcp_client(self, host_ip: str, port: int) -> None:
        """Connect to the host's TCP server (joiner side)."""
        try:
            conn = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            conn.settimeout(5)
            conn.connect((host_ip, port))
            conn.settimeout(None)
            with self._lock:
                self._peer_socks.append(conn)
            t = threading.Thread(target=self._recv_loop, args=(conn,), daemon=True)
            t.start()
            self._recv_threads.append(t)
            logger.info("Connected to collaboration host at %s:%d", host_ip, port)
        except OSError as exc:
            logger.warning("Cannot connect to host %s:%d: %s", host_ip, port, exc)
            raise

    def _accept_loop(self) -> None:
        """Background thread: accept incoming TCP peer connections."""
        assert self._server_sock is not None
        try:
            while True:
                try:
                    conn, addr = self._server_sock.accept()
                    logger.info("Peer connected from %s", addr)
                    with self._lock:
                        self._peer_socks.append(conn)
                    t = threading.Thread(target=self._recv_loop, args=(conn,), daemon=True)
                    t.start()
                    self._recv_threads.append(t)
                except OSError:
                    break
        except Exception as exc:
            logger.debug("Accept loop ended: %s", exc)

    def _recv_loop(self, sock: socket.socket) -> None:
        """Background thread: read length-prefixed JSON frames from *sock*."""
        _MAX_FRAME = 1_048_576  # 1 MB safety cap
        try:
            while True:
                # Read 4-byte header
                header = b""
                while len(header) < 4:
                    chunk = sock.recv(4 - len(header))
                    if not chunk:
                        return
                    header += chunk
                length = struct.unpack(">I", header)[0]
                if length > _MAX_FRAME:
                    logger.warning("Oversized collaboration frame (%d bytes) — closing.", length)
                    return
                # Read body
                body = b""
                while len(body) < length:
                    chunk = sock.recv(length - len(body))
                    if not chunk:
                        return
                    body += chunk
                try:
                    msg = json.loads(body.decode("utf-8"))
                    self._handle_incoming(msg)
                except (json.JSONDecodeError, UnicodeDecodeError) as exc:
                    logger.warning("Malformed collaboration frame: %s", exc)
        except OSError:
            pass
        finally:
            try:
                sock.close()
            except OSError:
                pass
            with self._lock:
                if sock in self._peer_socks:
                    self._peer_socks.remove(sock)

    def _handle_incoming(self, msg: dict) -> None:
        """Dispatch an incoming TCP message to event handlers."""
        try:
            msg_type = MessageType(msg.get("type", ""))
        except ValueError:
            logger.debug("Unknown collaboration message type: %s", msg.get("type"))
            return

        sender_id = msg.get("sender_id", "unknown")
        data = msg.get("data", {})
        ts = float(msg.get("timestamp", time.time()))

        record = CollaborativeMessage(
            msg_type=msg_type,
            sender_id=sender_id,
            timestamp=ts,
            data=data,
        )
        self.message_history.append(record)

        if msg_type == MessageType.CODE_UPDATE:
            change = CodeChange(
                user_id=sender_id,
                timestamp=ts,
                line=int(data.get("line", 0)),
                old_text=str(data.get("old_text", "")),
                new_text=str(data.get("new_text", "")),
                change_type="replace",
            )
            self.code_changes.append(change)
            self._trigger_event("code_changed", change)

        elif msg_type == MessageType.CURSOR_MOVE:
            if sender_id in self.participants:
                self.participants[sender_id].cursor_line = int(data.get("line", 0))
                self.participants[sender_id].cursor_col = int(data.get("col", 0))
            self._trigger_event("cursor_moved", data)

        elif msg_type == MessageType.CHAT:
            self._trigger_event("chat_message", str(data.get("content", "")))

        elif msg_type == MessageType.OUTPUT:
            self._trigger_event("output_received", str(data.get("content", "")))

        elif msg_type == MessageType.DISCONNECT:
            uid = data.get("user_id", sender_id)
            self.participants.pop(uid, None)
            self._trigger_event("participant_left", uid)

        elif msg_type == MessageType.SYNC_REQUEST:
            if self.is_host:
                self._send_message(MessageType.SYNC_RESPONSE, {"code": self.shared_code})

        elif msg_type == MessageType.SYNC_RESPONSE:
            self.shared_code = str(data.get("code", ""))
            self._trigger_event("sync_received", self.shared_code)

    def on_event(self, event_type: str, callback: Callable):
        """Register event callback."""
        if event_type not in self._callbacks:
            self._callbacks[event_type] = []
        self._callbacks[event_type].append(callback)

    def _trigger_event(self, event_type: str, *args):
        """Trigger event callbacks."""
        if event_type in self._callbacks:
            for callback in self._callbacks[event_type]:
                try:
                    callback(*args)
                except Exception as e:
                    logger.warning("Callback error: %s", e)


class SessionManager:
    """Manages multiple collaborative sessions."""

    def __init__(self, username: str):
        """Initialize session manager."""
        self.username = username
        self.current_session: Optional[LocalCollaborationSession] = None
        self.session_history: List[Dict] = []

    def create_session(self, code: str = "") -> LocalCollaborationSession:
        """Create new collaborative session."""
        session = LocalCollaborationSession(self.username)
        session.start_session(code)
        self.current_session = session
        return session

    def list_available_sessions(self, timeout: int = 5) -> List[Dict]:
        """List available sessions to join."""
        session = LocalCollaborationSession(self.username)
        sessions = session.discover_sessions(timeout)
        return sessions

    def join_session(
        self, host_user_id: str, host_ip: str, host_port: int
    ) -> Optional[LocalCollaborationSession]:
        """Join existing session."""
        session = LocalCollaborationSession(self.username)
        if session.join_session(host_user_id, host_ip, host_port):
            self.current_session = session
            return session
        return None

    def end_session(self):
        """End current session."""
        if self.current_session:
            log = self.current_session.export_session_log()
            self.session_history.append(log)
            self.current_session.disconnect()
            self.current_session = None
