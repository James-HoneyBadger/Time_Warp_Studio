"""
Database Configuration and Session Management
Handles SQLAlchemy setup and database connection pooling
"""

import logging
import os
from typing import Generator

from sqlalchemy.ext.asyncio import (
    AsyncSession,
    async_sessionmaker,
    create_async_engine,
)

logger = logging.getLogger(__name__)

# Database URL from environment or default
DATABASE_URL = os.getenv(
    "DATABASE_URL",
    "postgresql+asyncpg://timewarpadmin:timewarpadmin@localhost:5432/timewarproomdb",
)

# Validate SQLAlchemy 2.0+ async URL format
if not DATABASE_URL.startswith("postgresql+asyncpg://"):
    # Convert standard postgresql:// to asyncpg version
    DATABASE_URL = DATABASE_URL.replace(
        "postgresql://", "postgresql+asyncpg://"
    ).replace("postgres://", "postgresql+asyncpg://")

logger.info(f"Database URL: {DATABASE_URL.split('@')[0]}...")


# Create async engine
engine = create_async_engine(
    DATABASE_URL,
    echo=os.getenv("SQL_ECHO", "false").lower() == "true",
    pool_size=int(os.getenv("DB_POOL_SIZE", "10")),
    max_overflow=int(os.getenv("DB_MAX_OVERFLOW", "20")),
    pool_recycle=int(os.getenv("DB_POOL_RECYCLE", "3600")),
    pool_pre_ping=True,  # Test connections before using
    connect_args={
        "server_settings": {
            "jit": "off",  # Disable JIT for predictable performance
            "application_name": "TimeWarp_IDE",
        }
    },
)

# Create session factory
AsyncSessionLocal = async_sessionmaker(
    engine,
    class_=AsyncSession,
    expire_on_commit=False,
    autoflush=False,
)


async def get_session() -> Generator[AsyncSession, None, None]:
    """Dependency for FastAPI to get database session"""
    async with AsyncSessionLocal() as session:
        try:
            yield session
        finally:
            await session.close()


async def init_db():
    """Initialize database tables"""
    from .models import Base

    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
    logger.info("Database tables initialized")


async def close_db():
    """Close database connection"""
    await engine.dispose()
    logger.info("Database connection closed")


# Health check
async def check_db_health() -> bool:
    """Check database connectivity"""
    try:
        async with AsyncSessionLocal() as session:
            await session.execute("SELECT 1")
            return True
    except Exception as e:
        logger.error(f"Database health check failed: {e}")
        return False
