"""
Integration Tests for Backend API Routes
Tests REST endpoints and WebSocket integration
"""

import pytest
import pytest_asyncio
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import (
    AsyncSession,
    async_sessionmaker,
    create_async_engine,
)

from time_warp.db import get_session
from time_warp.main import app
from time_warp.models import Base

pytestmark = pytest.mark.asyncio


# Fixtures for test database and client
@pytest_asyncio.fixture
async def test_db():
    """Create test database"""
    # Use in-memory SQLite for testing (or test PostgreSQL)
    engine = create_async_engine(
        "sqlite+aiosqlite:///:memory:", echo=False, future=True
    )

    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)

    TestingSessionLocal = async_sessionmaker(
        engine, class_=AsyncSession, expire_on_commit=False
    )

    async def override_get_session():
        async with TestingSessionLocal() as session:
            yield session

    app.dependency_overrides[get_session] = override_get_session

    yield TestingSessionLocal

    await engine.dispose()


@pytest_asyncio.fixture
async def client(test_db):
    """Create test client"""
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as ac:
        yield ac


# Room API Tests
@pytest.mark.asyncio
async def test_create_room(client):
    """Test room creation"""
    response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["name"] == "Test Room"
    assert data["owner_id"] == "user123"


@pytest.mark.asyncio
async def test_get_room(client):
    """Test getting room details"""
    # Create room first
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Get room
    response = await client.get(f"/api/rooms/{room_id}")
    assert response.status_code == 200
    data = response.json()
    assert data["id"] == room_id


@pytest.mark.asyncio
async def test_add_room_member(client):
    """Test adding member to room"""
    # Create room
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Add member
    response = await client.post(
        f"/api/rooms/{room_id}/members",
        params={"user_id": "user456", "user_name": "John Doe"},
    )
    assert response.status_code == 200
    data = response.json()
    assert "member_id" in data


@pytest.mark.asyncio
async def test_get_room_members(client):
    """Test getting room members"""
    # Create room
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Add member
    await client.post(
        f"/api/rooms/{room_id}/members",
        params={"user_id": "user456", "user_name": "John Doe"},
    )

    # Get members
    response = await client.get(f"/api/rooms/{room_id}/members")
    assert response.status_code == 200
    data = response.json()
    assert len(data) >= 1


# User API Tests
@pytest.mark.asyncio
async def test_get_user_profile(client):
    """Test getting user profile"""
    response = await client.get("/api/users/user123/profile")
    assert response.status_code == 200
    data = response.json()
    assert data["id"] == "user123"
    assert data["email"] is not None


@pytest.mark.asyncio
async def test_get_user_rooms(client):
    """Test getting user rooms"""
    response = await client.get("/api/users/user123/rooms")
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)


@pytest.mark.asyncio
async def test_join_room(client):
    """Test user joining a room"""
    # Create room
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Join room
    response = await client.post(
        f"/api/users/user456/rooms/{room_id}/join",
        params={"username": "John Doe"},
    )
    assert response.status_code == 200
    data = response.json()
    assert "member_id" in data


# Sync API Tests
@pytest.mark.asyncio
async def test_record_operation(client):
    """Test recording an operation"""
    # Create room
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Record operation
    response = await client.post(
        f"/api/sync/{room_id}/operations",
        json={
            "user_id": "user123",
            "op_type": "insert",
            "position": 0,
            "content": "Hello",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert "operation_id" in data
    assert "version" in data


@pytest.mark.asyncio
async def test_get_current_version(client):
    """Test getting current document version"""
    # Create room
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Get version
    response = await client.get(f"/api/sync/{room_id}/version")
    assert response.status_code == 200
    data = response.json()
    assert "current_version" in data


@pytest.mark.asyncio
async def test_get_snapshot(client):
    """Test getting document snapshot"""
    # Create room
    create_response = await client.post(
        "/api/rooms",
        json={"name": "Test Room", "is_private": False},
        params={"owner_id": "user123"},
    )
    room_id = create_response.json()["id"]

    # Get snapshot
    response = await client.get(f"/api/sync/{room_id}/snapshot")
    assert response.status_code == 200
    data = response.json()
    assert "content" in data
    assert "version" in data


@pytest.mark.asyncio
async def test_health_check(client):
    """Test health check endpoint"""
    response = await client.get("/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "running"
    assert data["version"] == "4.5.2"


@pytest.mark.asyncio
async def test_root_endpoint(client):
    """Test root endpoint"""
    response = await client.get("/")
    assert response.status_code == 200
    data = response.json()
    assert "message" in data
    assert data["message"] == "Time Warp Studio Server"
