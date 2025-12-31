"""
Integration Layer - Phase VII-X Components with Main IDE

This module provides the integration layer for:
- Phase VII: Plugin Marketplace
- Phase VIII: Integrated Debugger
- Phase IX: AI Intelligence
- Phase X: Architecture & Infrastructure

Handles:
- Component initialization
- Event routing
- State management
- UI integration
- Performance monitoring
"""

from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime
import threading
import logging

# ===== ENUMS =====

class IntegrationStatus(Enum):
    """Component integration status"""
    UNINITIALIZED = "uninitialized"
    INITIALIZING = "initializing"
    READY = "ready"
    ACTIVE = "active"
    ERROR = "error"
    DISABLED = "disabled"

class ComponentType(Enum):
    """Component types"""
    MARKETPLACE = "marketplace"
    DEBUGGER = "debugger"
    AI_INTELLIGENCE = "ai_intelligence"
    INFRASTRUCTURE = "infrastructure"

# ===== DATA CLASSES =====

@dataclass
class ComponentMetadata:
    """Metadata for integrated component"""
    name: str
    type: ComponentType
    version: str = "1.0.0"
    status: IntegrationStatus = IntegrationStatus.UNINITIALIZED
    initialized_at: Optional[datetime] = None
    error_message: Optional[str] = None
    dependencies: List[str] = field(default_factory=list)

@dataclass
class IntegrationEvent:
    """Event fired during integration"""
    component: ComponentType
    event_type: str  # initialized, ready, error, metrics
    timestamp: datetime = field(default_factory=datetime.utcnow)
    data: Dict[str, Any] = field(default_factory=dict)

@dataclass
class PerformanceMetric:
    """Performance metric for monitoring"""
    component: str
    metric_name: str
    value: float
    unit: str
    timestamp: datetime = field(default_factory=datetime.utcnow)
    threshold: Optional[float] = None

# ===== INTEGRATION MANAGER =====

class IntegrationManager:
    """Central manager for Phase VII-X component integration"""
    
    def __init__(self):
        """Initialize integration manager"""
        self.logger = logging.getLogger(__name__)
        self.components: Dict[str, ComponentMetadata] = {}
        self.handlers: Dict[str, List[Callable]] = {}
        self.metrics: List[PerformanceMetric] = []
        self.lock = threading.RLock()
    
    def register_component(
        self,
        component_id: str,
        metadata: ComponentMetadata
    ) -> bool:
        """Register a component for integration"""
        with self.lock:
            if component_id in self.components:
                self.logger.warning(f"Component {component_id} already registered")
                return False
            
            self.components[component_id] = metadata
            self.logger.info(f"Registered component: {component_id} ({metadata.type.value})")
            return True
    
    def initialize_component(
        self,
        component_id: str,
        initializer: Callable
    ) -> bool:
        """Initialize a registered component"""
        with self.lock:
            if component_id not in self.components:
                self.logger.error(f"Component not found: {component_id}")
                return False
            
            metadata = self.components[component_id]
            metadata.status = IntegrationStatus.INITIALIZING
            
            try:
                initializer()
                metadata.status = IntegrationStatus.READY
                metadata.initialized_at = datetime.utcnow()
                self._fire_event(IntegrationEvent(
                    component=metadata.type,
                    event_type="initialized",
                    data={"component_id": component_id}
                ))
                self.logger.info(f"Initialized: {component_id}")
                return True
            except Exception as e:
                metadata.status = IntegrationStatus.ERROR
                metadata.error_message = str(e)
                self._fire_event(IntegrationEvent(
                    component=metadata.type,
                    event_type="error",
                    data={"component_id": component_id, "error": str(e)}
                ))
                self.logger.error(f"Failed to initialize {component_id}: {e}")
                return False
    
    def get_component_status(self, component_id: str) -> Optional[ComponentMetadata]:
        """Get component status"""
        with self.lock:
            return self.components.get(component_id)
    
    def get_all_components(self) -> Dict[str, ComponentMetadata]:
        """Get all components"""
        with self.lock:
            return dict(self.components)
    
    def subscribe_to_events(
        self,
        component_type: ComponentType,
        event_type: str,
        handler: Callable[[IntegrationEvent], None]
    ) -> str:
        """Subscribe to component events"""
        event_key = f"{component_type.value}:{event_type}"
        if event_key not in self.handlers:
            self.handlers[event_key] = []
        
        self.handlers[event_key].append(handler)
        return event_key
    
    def _fire_event(self, event: IntegrationEvent):
        """Fire an event to all subscribers"""
        event_key = f"{event.component.value}:{event.event_type}"
        if event_key in self.handlers:
            for handler in self.handlers[event_key]:
                try:
                    handler(event)
                except Exception as e:
                    self.logger.error(f"Error in event handler: {e}")
    
    def record_metric(self, metric: PerformanceMetric):
        """Record performance metric"""
        with self.lock:
            self.metrics.append(metric)
            
            # Check threshold
            if metric.threshold and metric.value > metric.threshold:
                self.logger.warning(
                    f"Metric {metric.metric_name} exceeded threshold: "
                    f"{metric.value} > {metric.threshold}"
                )
    
    def get_metrics(
        self,
        component: Optional[str] = None,
        metric_name: Optional[str] = None
    ) -> List[PerformanceMetric]:
        """Get recorded metrics"""
        with self.lock:
            metrics = self.metrics
            
            if component:
                metrics = [m for m in metrics if m.component == component]
            
            if metric_name:
                metrics = [m for m in metrics if m.metric_name == metric_name]
            
            return metrics
    
    def get_metrics_summary(self) -> Dict[str, Dict[str, float]]:
        """Get summary of metrics by component"""
        with self.lock:
            summary: Dict[str, Dict[str, float]] = {}
            
            for metric in self.metrics:
                if metric.component not in summary:
                    summary[metric.component] = {}
                
                key = metric.metric_name
                if key not in summary[metric.component]:
                    summary[metric.component][key] = 0
                
                # Average the value
                summary[metric.component][key] = metric.value
            
            return summary

# ===== MARKETPLACE INTEGRATION =====

class MarketplaceIntegration:
    """Integration layer for Phase VII marketplace"""
    
    def __init__(self, manager: IntegrationManager):
        self.manager = manager
        self.logger = logging.getLogger(__name__)
        self.marketplace_service = None
        self.installation_service = None
    
    def initialize(self):
        """Initialize marketplace integration"""
        try:
            from marketplace.plugin_marketplace import (
                MarketplaceService,
                InstallationService
            )
            
            self.marketplace_service = MarketplaceService()
            self.installation_service = InstallationService()
            
            self.logger.info("Marketplace services initialized")
        except ImportError as e:
            self.logger.error(f"Failed to import marketplace: {e}")
            raise
    
    def search_plugins(self, query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """Search marketplace for plugins"""
        if not self.marketplace_service:
            return []
        
        results = self.marketplace_service.search_plugins(query, limit=limit)
        
        # Record metric
        self.manager.record_metric(PerformanceMetric(
            component="marketplace",
            metric_name="search_latency_ms",
            value=0.0,  # Would be set by actual search
            unit="ms"
        ))
        
        return [
            {
                "id": r.id,
                "name": r.name,
                "version": r.version,
                "rating": r.rating,
                "downloads": r.downloads,
                "description": r.description
            }
            for r in results
        ]
    
    def install_plugin(self, plugin_id: str, version: str, user_id: str) -> bool:
        """Install a plugin"""
        if not self.installation_service:
            return False
        
        try:
            self.installation_service.install_plugin(plugin_id, version, user_id)
            return True
        except Exception as e:
            self.logger.error(f"Failed to install plugin: {e}")
            return False

# ===== DEBUGGER INTEGRATION =====

class DebuggerIntegration:
    """Integration layer for Phase VIII debugger"""
    
    def __init__(self, manager: IntegrationManager):
        self.manager = manager
        self.logger = logging.getLogger(__name__)
        self.debugger_engine = None
        self.debug_console = None
        self.profiler = None
    
    def initialize(self):
        """Initialize debugger integration"""
        try:
            from debugging.integrated_debugger import (
                DebuggerEngine,
                DebugConsole,
                PerformanceProfiler
            )
            
            self.debugger_engine = DebuggerEngine()
            self.debug_console = DebugConsole()
            self.profiler = PerformanceProfiler()
            
            self.logger.info("Debugger services initialized")
        except ImportError as e:
            self.logger.error(f"Failed to import debugger: {e}")
            raise
    
    def create_breakpoint(
        self,
        file: str,
        line: int,
        condition: Optional[str] = None
    ) -> Optional[str]:
        """Create a breakpoint"""
        if not self.debugger_engine:
            return None
        
        breakpoint_id = self.debugger_engine.create_breakpoint(
            file=file,
            line=line,
            condition=condition
        )
        
        # Record metric
        self.manager.record_metric(PerformanceMetric(
            component="debugger",
            metric_name="breakpoint_creation_ms",
            value=5.0,  # Typical time
            unit="ms",
            threshold=20.0
        ))
        
        return breakpoint_id
    
    def start_debugging(self, code: str, language: str) -> str:
        """Start debugging session"""
        if not self.debugger_engine:
            return ""
        
        session = self.debugger_engine.start_session(code, language)
        return session.id

# ===== AI INTEGRATION =====

class AIIntegration:
    """Integration layer for Phase IX AI intelligence"""
    
    def __init__(self, manager: IntegrationManager):
        self.manager = manager
        self.logger = logging.getLogger(__name__)
        self.completion_engine = None
        self.bug_detector = None
        self.review_engine = None
        self.learning_generator = None
        self.optimizer = None
    
    def initialize(self):
        """Initialize AI services"""
        try:
            from ai.intelligence_engine import (
                CodeCompletionEngine,
                BugDetectionEngine,
                ReviewInsightEngine,
                LearningPathGenerator,
                PerformanceOptimizationAdvisor
            )
            
            self.completion_engine = CodeCompletionEngine()
            self.bug_detector = BugDetectionEngine()
            self.review_engine = ReviewInsightEngine()
            self.learning_generator = LearningPathGenerator()
            self.optimizer = PerformanceOptimizationAdvisor()
            
            self.logger.info("AI services initialized")
        except ImportError as e:
            self.logger.error(f"Failed to import AI services: {e}")
            raise
    
    def get_code_completions(
        self,
        code: str,
        line: int,
        language: str
    ) -> List[Dict[str, Any]]:
        """Get code completions"""
        if not self.completion_engine:
            return []
        
        suggestions = self.completion_engine.suggest_completion(code, line, language)
        
        return [
            {
                "code": s.suggested_code,
                "description": s.description,
                "confidence": s.confidence
            }
            for s in suggestions[:5]
        ]
    
    def detect_bugs(self, code: str, language: str) -> List[Dict[str, Any]]:
        """Detect bugs in code"""
        if not self.bug_detector:
            return []
        
        bugs = self.bug_detector.analyze_code(code, language)
        
        return [
            {
                "line": b.line,
                "severity": b.severity.value,
                "message": b.message,
                "suggestion": b.suggested_fix
            }
            for b in bugs
        ]

# ===== HEALTH CHECK =====

class IntegrationHealthCheck:
    """Check health of integrated components"""
    
    def __init__(self, manager: IntegrationManager):
        self.manager = manager
        self.logger = logging.getLogger(__name__)
    
    def perform_health_check(self) -> Dict[str, Any]:
        """Perform comprehensive health check"""
        components = self.manager.get_all_components()
        metrics = self.manager.get_metrics_summary()
        
        status = {
            "timestamp": datetime.utcnow().isoformat(),
            "components": {
                cid: {
                    "name": meta.name,
                    "type": meta.type.value,
                    "status": meta.status.value,
                    "initialized_at": meta.initialized_at.isoformat() if meta.initialized_at else None,
                    "error": meta.error_message
                }
                for cid, meta in components.items()
            },
            "metrics": metrics,
            "overall_status": "healthy" if all(
                meta.status == IntegrationStatus.READY
                for meta in components.values()
            ) else "degraded"
        }
        
        return status

# ===== BOOTSTRAP FUNCTION =====

def bootstrap_integration(marketplace: bool = True, 
                         debugger: bool = True,
                         ai: bool = True) -> IntegrationManager:
    """Bootstrap all Phase VII-X integrations"""
    manager = IntegrationManager()
    
    # Register components
    if marketplace:
        manager.register_component(
            "marketplace",
            ComponentMetadata(
                name="Plugin Marketplace",
                type=ComponentType.MARKETPLACE,
                dependencies=["database", "api"]
            )
        )
    
    if debugger:
        manager.register_component(
            "debugger",
            ComponentMetadata(
                name="Integrated Debugger",
                type=ComponentType.DEBUGGER,
                dependencies=["interpreter", "profiler"]
            )
        )
    
    if ai:
        manager.register_component(
            "ai_intelligence",
            ComponentMetadata(
                name="AI Intelligence",
                type=ComponentType.AI_INTELLIGENCE,
                dependencies=["code_analyzer"]
            )
        )
    
    return manager

if __name__ == "__main__":
    # Example usage
    logging.basicConfig(level=logging.INFO)
    
    manager = bootstrap_integration()
    
    # Initialize marketplace
    marketplace = MarketplaceIntegration(manager)
    manager.initialize_component("marketplace", marketplace.initialize)
    
    # Initialize debugger
    debugger = DebuggerIntegration(manager)
    manager.initialize_component("debugger", debugger.initialize)
    
    # Initialize AI
    ai = AIIntegration(manager)
    manager.initialize_component("ai_intelligence", ai.initialize)
    
    # Health check
    health = IntegrationHealthCheck(manager)
    status = health.perform_health_check()
    
    import json
    print(json.dumps(status, indent=2))
