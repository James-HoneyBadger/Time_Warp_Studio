"""
Time Warp Studio - System Integration & Orchestration

Provides:
- Central orchestration of all subsystems
- Startup and initialization sequencing
- Cross-component communication
- System health and status reporting
- Version management and upgrades
"""

import sys
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

# ===== ENUMS =====


class SystemStatus(Enum):
    """System operational status"""

    INITIALIZING = "initializing"
    RUNNING = "running"
    DEGRADED = "degraded"
    SHUTTING_DOWN = "shutting_down"
    FAILED = "failed"


class ComponentStatus(Enum):
    """Individual component status"""

    UNINITIALIZED = "uninitialized"
    INITIALIZING = "initializing"
    READY = "ready"
    BUSY = "busy"
    FAILED = "failed"
    DISABLED = "disabled"


# ===== DATA CLASSES =====


@dataclass
class ComponentInfo:
    """Information about a system component"""

    name: str
    version: str
    status: ComponentStatus = ComponentStatus.UNINITIALIZED
    dependencies: List[str] = field(default_factory=list)
    last_initialized: Optional[datetime] = None
    last_error: Optional[str] = None
    health_check_interval_seconds: int = 60


@dataclass
class SystemInfo:
    """Overall system information"""

    version: str = "6.1.0"
    status: SystemStatus = SystemStatus.INITIALIZING
    uptime_seconds: int = 0
    started_at: datetime = field(default_factory=datetime.utcnow)
    components: Dict[str, ComponentInfo] = field(default_factory=dict)

    # Subsystem versions
    python_version: str = ""
    pyside_version: str = ""
    supported_languages: List[str] = field(default_factory=list)

    # Environment
    data_path: str = ""
    config_path: str = ""
    cache_path: str = ""


@dataclass
class InitializationReport:
    """Report from system initialization"""

    success: bool = True
    timestamp: datetime = field(default_factory=datetime.utcnow)
    components_initialized: int = 0
    components_failed: int = 0
    initialization_time_seconds: float = 0.0
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)


# ===== COMPONENT REGISTRY =====


class ComponentRegistry:
    """Manages all system components"""

    def __init__(self):
        self.components: Dict[str, Any] = {}
        self.initializers: Dict[str, Callable] = {}
        self.shutdown_handlers: Dict[str, Callable] = {}

    def register_component(
        self,
        name: str,
        component: Any,
        initializer: Optional[Callable] = None,
        shutdown_handler: Optional[Callable] = None,
        version: str = "1.0.0",
        dependencies: List[str] = None,
    ) -> None:
        """Register a system component"""
        self.components[name] = component
        if initializer:
            self.initializers[name] = initializer
        if shutdown_handler:
            self.shutdown_handlers[name] = shutdown_handler

    def get_component(self, name: str) -> Optional[Any]:
        """Get component instance"""
        return self.components.get(name)

    def list_components(self) -> List[str]:
        """List all registered components"""
        return list(self.components.keys())


# ===== SYSTEM ORCHESTRATOR =====


class SystemOrchestrator:
    """Orchestrates all system components and operations"""

    def __init__(self):
        self.registry = ComponentRegistry()
        self.system_info = SystemInfo()
        self.initialization_report = InitializationReport()
        self.status_callbacks: List[Callable[[SystemStatus], None]] = []

        # Initialize Python/environment info
        self.system_info.python_version = f"{
            sys.version_info.major}.{
            sys.version_info.minor}.{
            sys.version_info.micro}"

    def register_component(
        self,
        name: str,
        component: Any,
        version: str = "1.0.0",
        dependencies: List[str] = None,
        initializer: Optional[Callable] = None,
        shutdown_handler: Optional[Callable] = None,
    ) -> bool:
        """Register a system component"""
        try:
            self.registry.register_component(
                name,
                component,
                initializer,
                shutdown_handler,
                version,
                dependencies or [],
            )

            info = ComponentInfo(
                name=name, version=version, dependencies=dependencies or []
            )
            self.system_info.components[name] = info
            return True
        except Exception as e:
            self.initialization_report.errors.append(
                f"Failed to register {name}: {str(e)}"
            )
            return False

    def initialize_system(self) -> InitializationReport:
        """Initialize all system components"""
        self._set_status(SystemStatus.INITIALIZING)
        start_time = datetime.utcnow()

        try:
            # Set default supported languages
            self.system_info.supported_languages = [
                "BASIC",
                "PILOT",
                "Logo",
                "Pascal",
                "Prolog",
                "C",
                "Forth",
                "Ruby",
                "JavaScript",
            ]

            # Initialize components
            for name, initializer in self.registry.initializers.items():
                try:
                    initializer()
                    component_info = self.system_info.components.get(name)
                    if component_info:
                        component_info.status = ComponentStatus.READY
                        component_info.last_initialized = datetime.utcnow()
                    self.initialization_report.components_initialized += 1
                except Exception as e:
                    self.initialization_report.errors.append(f"{name}: {str(e)}")
                    self.initialization_report.components_failed += 1
                    component_info = self.system_info.components.get(name)
                    if component_info:
                        component_info.status = ComponentStatus.FAILED
                        component_info.last_error = str(e)

            # Calculate initialization time
            init_time = (datetime.utcnow() - start_time).total_seconds()
            self.initialization_report.initialization_time_seconds = init_time

            # Determine overall success
            if self.initialization_report.components_failed == 0:
                self.initialization_report.success = True
                self._set_status(SystemStatus.RUNNING)
            else:
                self.initialization_report.success = False
                self._set_status(SystemStatus.DEGRADED)

            return self.initialization_report

        except Exception as e:
            self.initialization_report.success = False
            self.initialization_report.errors.append(
                f"System initialization failed: {str(e)}"
            )
            self._set_status(SystemStatus.FAILED)
            return self.initialization_report

    def shutdown_system(self) -> bool:
        """Gracefully shutdown all components"""
        self._set_status(SystemStatus.SHUTTING_DOWN)

        try:
            # Call shutdown handlers in reverse order
            for name in reversed(list(self.registry.shutdown_handlers.keys())):
                try:
                    shutdown_handler = self.registry.shutdown_handlers[name]
                    shutdown_handler()
                except Exception as e:
                    self.initialization_report.warnings.append(
                        f"Error shutting down {name}: {str(e)}"
                    )

            return True
        except (ValueError, TypeError):
            self._set_status(SystemStatus.FAILED)
            return False

    def get_system_status(self) -> Dict[str, Any]:
        """Get current system status"""
        return {
            "status": self.system_info.status.value,
            "version": self.system_info.version,
            "uptime_seconds": (
                datetime.utcnow() - self.system_info.started_at
            ).total_seconds(),
            "components": {
                name: {
                    "version": info.version,
                    "status": info.status.value,
                    "last_error": info.last_error,
                }
                for name, info in self.system_info.components.items()
            },
            "supported_languages": self.system_info.supported_languages,
            "python_version": self.system_info.python_version,
        }

    def get_component(self, name: str) -> Optional[Any]:
        """Get component instance"""
        return self.registry.get_component(name)

    def register_status_callback(
        self, callback: Callable[[SystemStatus], None]
    ) -> None:
        """Register callback for status changes"""
        self.status_callbacks.append(callback)

    def _set_status(self, status: SystemStatus) -> None:
        """Update system status and notify callbacks"""
        self.system_info.status = status
        for callback in self.status_callbacks:
            try:
                callback(status)
            except Exception as e:
                print(f"Error in status callback: {e}")


# ===== GLOBAL ORCHESTRATOR INSTANCE =====

_global_orchestrator: Optional[SystemOrchestrator] = None


def get_system_orchestrator() -> SystemOrchestrator:
    """Get global system orchestrator"""
    global _global_orchestrator
    if _global_orchestrator is None:
        _global_orchestrator = SystemOrchestrator()
    return _global_orchestrator


def initialize_system() -> InitializationReport:
    """Initialize Time Warp Studio"""
    orchestrator = get_system_orchestrator()
    return orchestrator.initialize_system()


def shutdown_system() -> bool:
    """Shutdown Time Warp Studio"""
    orchestrator = get_system_orchestrator()
    return orchestrator.shutdown_system()


def get_system_info() -> Dict[str, Any]:
    """Get system information"""
    orchestrator = get_system_orchestrator()
    return orchestrator.get_system_status()


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    # Get orchestrator
    orchestrator = get_system_orchestrator()

    # Register mock components
    class MockInterpreter:
        def initialize(self):
            pass

        def shutdown(self):
            pass

    class MockUI:
        def initialize(self):
            pass

        def shutdown(self):
            pass

    class MockCollaboration:
        def initialize(self):
            pass

        def shutdown(self):
            pass

    # Register components
    interpreter = MockInterpreter()
    ui = MockUI()
    collab = MockCollaboration()

    orchestrator.register_component(
        "interpreter", interpreter, initializer=interpreter.initialize
    )
    orchestrator.register_component("ui", ui, initializer=ui.initialize)
    orchestrator.register_component(
        "collaboration", collab, initializer=collab.initialize
    )

    # Initialize system
    print("🚀 Initializing Time Warp Studio...")
    report = orchestrator.initialize_system()

    print(f"✅ Initialization completed in {
            report.initialization_time_seconds:.2f}s")
    print(f"✅ Components initialized: {report.components_initialized}")
    if report.components_failed > 0:
        print(f"⚠️  Components failed: {report.components_failed}")

    # Display system status
    print("\n📊 System Status:")
    status = orchestrator.get_system_status()
    print(f"  Status: {status['status']}")
    print(f"  Version: {status['version']}")
    print(f"  Python: {status['python_version']}")
    print(f"  Languages: {', '.join(status['supported_languages'])}")

    print("\n📦 Component Status:")
    for name, component_status in status["components"].items():
        print(f"  {name}: {component_status['status']}")

    # Shutdown
    print("\n🛑 Shutting down...")
    orchestrator.shutdown_system()
    print("✅ Shutdown complete")
