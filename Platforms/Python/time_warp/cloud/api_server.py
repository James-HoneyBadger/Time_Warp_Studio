"""Time Warp Cloud Backend API Server - FastAPI Implementation.

This module provides the cloud backend services for Time Warp Studio v6.1.0,
including REST APIs, WebSocket support, and real-time multiplayer features.
"""

import logging
import secrets
import uuid
from datetime import datetime, timedelta, timezone
from enum import Enum
from typing import Any, Dict, List, Optional

import jwt
from fastapi import Depends, FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer
from pydantic import BaseModel, EmailStr, Field

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# ============================================================================
# DATA MODELS
# ============================================================================


class UserRole(str, Enum):
    """User roles in Time Warp Cloud."""

    STUDENT = "student"
    TEACHER = "teacher"
    DEVELOPER = "developer"
    ADMIN = "admin"


class ProjectLanguage(str, Enum):
    """Supported programming languages."""

    BASIC = "basic"
    LOGO = "logo"
    PILOT = "pilot"
    PASCAL = "pascal"
    C = "c"
    FORTH = "forth"
    PROLOG = "prolog"


class SyncStatus(str, Enum):
    """Project sync status."""

    SYNCED = "synced"
    PENDING = "pending"
    CONFLICTED = "conflicted"
    SYNCING = "syncing"


class SessionMode(str, Enum):
    """Multiplayer session modes."""

    PAIR = "pair"
    GROUP = "group"
    CLASSROOM = "classroom"
    TOURNAMENT = "tournament"


# Request/Response Models
class UserCreate(BaseModel):
    """User registration request."""

    username: str = Field(..., min_length=3, max_length=50)
    email: EmailStr
    password: str = Field(..., min_length=8)
    full_name: Optional[str] = None
    role: UserRole = UserRole.STUDENT


class UserResponse(BaseModel):
    """User response model."""

    id: str
    username: str
    email: str
    full_name: Optional[str]
    role: UserRole
    created_at: datetime
    cloud_sync_enabled: bool = False
    max_projects: int = 10


class LoginRequest(BaseModel):
    """Login request model."""

    email: str
    password: str


class TokenResponse(BaseModel):
    """Token response model."""

    access_token: str
    refresh_token: str
    token_type: str = "bearer"
    expires_in: int


class ProjectCreate(BaseModel):
    """Create project request."""

    name: str = Field(..., min_length=1, max_length=100)
    description: Optional[str] = None
    language: ProjectLanguage
    is_public: bool = False


class ProjectUpdate(BaseModel):
    """Update project request."""

    name: Optional[str] = None
    description: Optional[str] = None
    is_public: Optional[bool] = None


class ProjectResponse(BaseModel):
    """Project response model."""

    id: str
    name: str
    description: Optional[str]
    language: ProjectLanguage
    owner_id: str
    is_public: bool
    created_at: datetime
    updated_at: datetime
    sync_status: SyncStatus = SyncStatus.SYNCED
    version: int = 1


class FileContent(BaseModel):
    """File content model."""

    id: str
    project_id: str
    filename: str
    content: str
    language: ProjectLanguage
    created_at: datetime
    updated_at: datetime
    version: int


class SessionCreate(BaseModel):
    """Create multiplayer session."""

    name: str = Field(..., min_length=1, max_length=100)
    mode: SessionMode
    max_participants: int = Field(2, ge=2, le=100)
    project_id: Optional[str] = None


class SessionResponse(BaseModel):
    """Session response model."""

    id: str
    name: str
    mode: SessionMode
    creator_id: str
    participants: List[str]
    max_participants: int
    created_at: datetime
    project_id: Optional[str]
    is_active: bool = True


class AchievementResponse(BaseModel):
    """Achievement response model."""

    id: str
    user_id: str
    name: str
    description: str
    points: int
    unacked_at: datetime


class LeaderboardEntry(BaseModel):
    """Leaderboard entry model."""

    rank: int
    user_id: str
    username: str
    points: int
    projects_completed: int
    sessions_participated: int


# ============================================================================
# AUTHENTICATION & SECURITY
# ============================================================================


class CloudAuthManager:
    """Manages authentication and JWT tokens."""

    def __init__(self, secret_key: str | None = None):
        """Initialize auth manager.

        Args:
            secret_key: JWT secret key (should be from env in production)
        """
        self.secret_key = secret_key or secrets.token_urlsafe(32)
        self.algorithm = "HS256"
        self.access_token_expire = 3600  # 1 hour
        self.refresh_token_expire = 86400 * 7  # 7 days

    def create_tokens(self, user_id: str) -> Dict[str, str]:
        """Create access and refresh tokens.

        Args:
            user_id: User ID to encode in token

        Returns:
            Dictionary with access_token and refresh_token
        """
        now = datetime.now(timezone.utc)

        # Access token
        access_payload = {
            "sub": user_id,
            "type": "access",
            "iat": now,
            "exp": now + timedelta(seconds=self.access_token_expire),
        }
        access_token = jwt.encode(
            access_payload, self.secret_key, algorithm=self.algorithm
        )

        # Refresh token
        refresh_payload = {
            "sub": user_id,
            "type": "refresh",
            "iat": now,
            "exp": now + timedelta(seconds=self.refresh_token_expire),
        }
        refresh_token = jwt.encode(
            refresh_payload, self.secret_key, algorithm=self.algorithm
        )

        return {
            "access_token": access_token,
            "refresh_token": refresh_token,
            "expires_in": self.access_token_expire,
        }

    def verify_token(self, token: str) -> Dict[str, Any]:
        """Verify JWT token.

        Args:
            token: JWT token to verify

        Returns:
            Decoded token payload

        Raises:
            HTTPException: If token is invalid
        """
        try:
            payload = jwt.decode(token, self.secret_key, algorithms=[self.algorithm])
            return payload
        except jwt.ExpiredSignatureError:
            raise HTTPException(status_code=401, detail="Token expired")
        except jwt.InvalidTokenError:
            raise HTTPException(status_code=401, detail="Invalid token")


# ============================================================================
# CLOUD API SERVER
# ============================================================================


class TimeWarpCloudAPI:
    """Main Time Warp Cloud API application."""

    def __init__(self, secret_key: str | None = None):
        """Initialize Cloud API.

        Args:
            secret_key: JWT secret key
        """
        self.app = FastAPI(
            title="Time Warp Cloud API",
            description="Cloud backend for Time Warp Studio v6.1.0",
            version="6.1.0",
        )

        # Configure CORS
        self.app.add_middleware(
            CORSMiddleware,
            allow_origins=["*"],  # Configure properly in production
            allow_credentials=True,
            allow_methods=["*"],
            allow_headers=["*"],
        )

        # Initialize components
        self.auth_manager = CloudAuthManager(secret_key)

        # In-memory storage (replace with database in production)
        self.users = {}
        self.projects = {}
        self.files = {}
        self.sessions = {}
        self.achievements = {}
        self.websocket_connections = {}

        # Setup routes
        self._setup_routes()

    def _create_user(self, user_data: Dict[str, Any]) -> str:
        """Create a user in storage (internal helper).

        Args:
            user_data: User data dict with username, email, password, role, full_name

        Returns:
            User ID
        """
        user_id = str(len(self.users) + 1)
        self.users[user_data["email"]] = {
            "id": user_id,
            "username": user_data["username"],
            "email": user_data["email"],
            "password": user_data["password"],
            "full_name": user_data.get("full_name"),
            "role": user_data.get("role", "student"),
            "created_at": datetime.now(timezone.utc).isoformat(),
            "cloud_sync_enabled": False,
            "max_projects": 10,
        }
        return user_id

    def _setup_routes(self):
        """Setup API routes."""

        # Health check
        @self.app.get("/health")
        async def health_check():
            return {"status": "healthy", "version": "6.1.0"}

        # ====================================================================
        # AUTHENTICATION ENDPOINTS
        # ====================================================================

        @self.app.post("/api/v1/auth/register", response_model=UserResponse)
        async def register(user: UserCreate):
            """Register new user."""
            if user.email in [u["email"] for u in self.users.values()]:
                raise HTTPException(status_code=400, detail="Email already registered")

            user_id = str(uuid.uuid4())
            self.users[user_id] = {
                "id": user_id,
                "username": user.username,
                "email": user.email,
                "password": user.password,  # Hash in production!
                "full_name": user.full_name,
                "role": user.role,
                "created_at": datetime.now(timezone.utc),
                "cloud_sync_enabled": False,
                "max_projects": 10,
            }

            return self.users[user_id]

        @self.app.post("/api/v1/auth/login", response_model=TokenResponse)
        async def login(creds: LoginRequest):
            """Login user."""
            user = None
            for u in self.users.values():
                if u["email"] == creds.email:
                    user = u
                    break

            if not user or user["password"] != creds.password:
                raise HTTPException(status_code=401, detail="Invalid credentials")

            tokens = self.auth_manager.create_tokens(user["id"])
            return TokenResponse(**tokens)

        @self.app.get("/api/v1/auth/me", response_model=UserResponse)
        async def get_current_user(
            auth: HTTPAuthorizationCredentials = Depends(HTTPBearer()),
        ):
            """Get current user profile."""
            payload = self.auth_manager.verify_token(auth.credentials)
            user_id = payload.get("sub")

            if user_id not in self.users:
                raise HTTPException(status_code=404, detail="User not found")

            return self.users[user_id]

        # ====================================================================
        # PROJECT ENDPOINTS
        # ====================================================================

        @self.app.get("/api/v1/projects")
        async def list_projects(
            auth: HTTPAuthorizationCredentials = Depends(HTTPBearer()),
        ):
            """List user's projects."""
            payload = self.auth_manager.verify_token(auth.credentials)
            user_id = payload.get("sub")

            user_projects = [
                p for p in self.projects.values() if p["owner_id"] == user_id
            ]
            return user_projects

        @self.app.post("/api/v1/projects", response_model=ProjectResponse)
        async def create_project(
            project: ProjectCreate,
            auth: HTTPAuthorizationCredentials = Depends(HTTPBearer()),
        ):
            """Create new project."""
            payload = self.auth_manager.verify_token(auth.credentials)
            user_id = payload.get("sub")

            project_id = str(uuid.uuid4())
            now = datetime.now(timezone.utc)

            self.projects[project_id] = {
                "id": project_id,
                "name": project.name,
                "description": project.description,
                "language": project.language,
                "owner_id": user_id,
                "is_public": project.is_public,
                "created_at": now,
                "updated_at": now,
                "sync_status": SyncStatus.SYNCED,
                "version": 1,
            }

            return self.projects[project_id]

        @self.app.get("/api/v1/projects/{project_id}", response_model=ProjectResponse)
        async def get_project(
            project_id: str,
            auth: HTTPAuthorizationCredentials = Depends(HTTPBearer()),
        ):
            """Get project details."""
            if project_id not in self.projects:
                raise HTTPException(status_code=404, detail="Project not found")

            return self.projects[project_id]

        @self.app.put("/api/v1/projects/{project_id}", response_model=ProjectResponse)
        async def update_project(
            project_id: str,
            update: ProjectUpdate,
            auth: HTTPAuthorizationCredentials = Depends(HTTPBearer()),
        ):
            """Update project."""
            if project_id not in self.projects:
                raise HTTPException(status_code=404, detail="Project not found")

            project = self.projects[project_id]
            if update.name:
                project["name"] = update.name
            if update.description is not None:
                project["description"] = update.description
            if update.is_public is not None:
                project["is_public"] = update.is_public

            project["updated_at"] = datetime.now(timezone.utc)
            project["version"] += 1

            return project

        # ====================================================================
        # MULTIPLAYER ENDPOINTS
        # ====================================================================

        @self.app.post("/api/v1/multiplayer/sessions", response_model=SessionResponse)
        async def create_session(
            session: SessionCreate,
            auth: HTTPAuthorizationCredentials = Depends(HTTPBearer()),
        ):
            """Create multiplayer session."""
            payload = self.auth_manager.verify_token(auth.credentials)
            user_id = payload.get("sub")

            session_id = str(uuid.uuid4())
            now = datetime.now(timezone.utc)

            self.sessions[session_id] = {
                "id": session_id,
                "name": session.name,
                "mode": session.mode,
                "creator_id": user_id,
                "participants": [user_id],
                "max_participants": session.max_participants,
                "created_at": now,
                "project_id": session.project_id,
                "is_active": True,
            }

            return self.sessions[session_id]

        @self.app.get("/api/v1/multiplayer/sessions/{session_id}")
        async def get_session(session_id: str):
            """Get session details."""
            if session_id not in self.sessions:
                raise HTTPException(status_code=404, detail="Session not found")

            return self.sessions[session_id]

        # ====================================================================
        # LEADERBOARD ENDPOINTS
        # ====================================================================

        @self.app.get("/api/v1/leaderboard")
        async def get_leaderboard(limit: int = 100):
            """Get global leaderboard."""
            # In production, this would query database with proper ordering
            leaderboard = [
                LeaderboardEntry(
                    rank=i + 1,
                    user_id=user_id,
                    username=user["username"],
                    points=len(
                        [p for p in self.projects.values() if p["owner_id"] == user_id]
                    )
                    * 100,
                    projects_completed=len(
                        [p for p in self.projects.values() if p["owner_id"] == user_id]
                    ),
                    sessions_participated=0,
                )
                for i, (user_id, user) in enumerate(list(self.users.items())[:limit])
            ]

            return leaderboard

        @self.app.get("/api/v1/leaderboard/user/{user_id}")
        async def get_user_stats(user_id: str):
            """Get user statistics."""
            if user_id not in self.users:
                raise HTTPException(status_code=404, detail="User not found")

            user_projects = [
                p for p in self.projects.values() if p["owner_id"] == user_id
            ]

            return {
                "user_id": user_id,
                "username": self.users[user_id]["username"],
                "projects_created": len(user_projects),
                "total_points": len(user_projects) * 100,
                "achievements": [
                    a for a in self.achievements.values() if a["user_id"] == user_id
                ],
                "joined_date": self.users[user_id]["created_at"],
            }

    def get_app(self):
        """Get FastAPI application instance."""
        return self.app


# ============================================================================
# APPLICATION FACTORY
# ============================================================================


def create_cloud_api(secret_key: str | None = None) -> FastAPI:
    """Create and configure Time Warp Cloud API.

    Args:
        secret_key: JWT secret key (optional)

    Returns:
        Configured FastAPI application
    """
    api = TimeWarpCloudAPI(secret_key)
    return api.get_app()


if __name__ == "__main__":
    import uvicorn

    app = create_cloud_api()
    logger.info("Starting Time Warp Cloud API on http://localhost:8000")
    uvicorn.run(app, host="0.0.0.0", port=8000)
