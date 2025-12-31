"""
Socket.io Server Configuration and Setup
Initializes Socket.io server with FastAPI integration
"""

import logging
from contextlib import asynccontextmanager
from typing import Any, Dict

from fastapi import FastAPI
from python_engineio import AsyncEngineIO
from python_socketio import AsyncServer

logger = logging.getLogger(__name__)


class SocketIOManager:
    """Manages Socket.io server instance and lifecycle"""

    def __init__(self, app: FastAPI = None, cors_origins: list = None):
        self.app = app
        self.socketio = None
        self.engine = None
        self.cors_origins = cors_origins or ["*"]

    def init_socketio(self):
        """Initialize Socket.io server with async engine"""
        self.engine = AsyncEngineIO(
            async_mode="asgi",
            cors_allowed_origins=self.cors_origins,
            ping_timeout=60,
            ping_interval=25,
            max_http_buffer_size=1e6,
        )

        self.socketio = AsyncServer(
            async_mode="asgi",
            engineio=self.engine,
            cors_allowed_origins=self.cors_origins,
            ping_timeout=60,
            ping_interval=25,
        )

        logger.info("Socket.io server initialized")
        return self.socketio

    def get_asgi_app(self):
        """Get ASGI app for mounting with FastAPI"""
        if not self.socketio:
            self.init_socketio()

        return self.socketio.to_asgi()

    def register_handlers(self, handler_class):
        """Register event handlers"""
        if not self.socketio:
            self.init_socketio()

        handler = handler_class(self.socketio)
        handler.register_all()
        logger.info(f"Registered handlers from {handler_class.__name__}")

    async def connect_server(self):
        """Called when server starts"""
        logger.info("Socket.io server started")

    async def disconnect_server(self):
        """Called when server stops"""
        logger.info("Socket.io server stopped")


# Singleton instance
_socketio_manager = None


def get_socketio_manager(app: FastAPI = None) -> SocketIOManager:
    """Get or create Socket.io manager"""
    global _socketio_manager
    if _socketio_manager is None:
        _socketio_manager = SocketIOManager(app)
        _socketio_manager.init_socketio()
    return _socketio_manager


@asynccontextmanager
async def lifespan(app: FastAPI):
    """FastAPI lifespan context manager"""
    manager = get_socketio_manager(app)
    await manager.connect_server()
    yield
    await manager.disconnect_server()
