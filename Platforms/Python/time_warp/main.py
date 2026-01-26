"""
Main FastAPI Application
Integrates WebSocket, REST API, and database components
"""

import logging
import os
from contextlib import asynccontextmanager
from typing import AsyncGenerator

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from .db import engine, get_session
from .routes import rooms, sync, users
from .socketio_config import SocketIOManager

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Lifespan context manager for startup/shutdown
@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator:
    """
    Manage application lifecycle
    """
    # Startup
    logger.info("Starting Time Warp Studio Server...")

    # Initialize database
    try:
        async with engine.begin() as conn:
            # Create all tables
            from .models import Base

            await conn.run_sync(Base.metadata.create_all)
            logger.info("Database tables created")
    except Exception as e:
        logger.error("Database initialization error: %s", e)

    # Initialize Socket.io
    sio_manager = SocketIOManager(app)
    await sio_manager.initialize()
    logger.info("Socket.io initialized")

    yield

    # Shutdown
    logger.info("Shutting down Time Warp Studio Server...")
    await engine.dispose()
    logger.info("Database connection closed")


# Create FastAPI application
app = FastAPI(
    title="Time Warp Studio Server",
    description="Real-time collaborative programming IDE backend",
    version="4.5.2",
    lifespan=lifespan,
)

# Configure CORS
origins = os.getenv(
    "CORS_ORIGINS", "http://localhost:3000,http://localhost:5173"
).split(",")

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routers
app.include_router(rooms.router)
app.include_router(users.router)
app.include_router(sync.router)


# Health check endpoint
@app.get("/health")
async def health_check():
    """Health check endpoint"""
    try:
        # Check database connection
        async with get_session() as session:
            await session.execute("SELECT 1")
        db_status = "healthy"
    except Exception as e:
        logger.error("Database health check failed: %s", e)
        db_status = "unhealthy"

    return {
        "status": "running",
        "version": "4.5.2",
        "database": db_status,
    }


# Root endpoint
@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "message": "Time Warp Studio Server",
        "version": "4.5.2",
        "docs": "/docs",
        "health": "/health",
    }


# Serve static files if they exist
try:
    app.mount("/static", StaticFiles(directory="static"), name="static")
except Exception as e:
    logger.warning("Static files not mounted: %s", e)


if __name__ == "__main__":
    import uvicorn

    # Load environment variables
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", 8000))
    workers = int(os.getenv("WORKERS", 1))

    # For development, use reload
    reload = os.getenv("ENV", "development") == "development"

    logger.info("Starting server on %s:{port}", host)

    uvicorn.run(
        "main:app",
        host=host,
        port=port,
        reload=reload,
        workers=workers,
        log_level="info",
    )
