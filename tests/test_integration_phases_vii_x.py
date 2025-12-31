"""
Integration Tests for Phase VII-X Components

Tests:
- Marketplace integration
- Debugger integration
- AI intelligence integration
- Component interaction
- Performance benchmarks
- End-to-end workflows
"""

import pytest
import sys
import os
from datetime import datetime
from typing import Dict, List
from unittest.mock import Mock, patch, MagicMock

# Add project to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../..'))

from integration.integration_manager import (
    IntegrationManager,
    ComponentMetadata,
    ComponentType,
    IntegrationStatus,
    IntegrationEvent,
    PerformanceMetric,
    MarketplaceIntegration,
    DebuggerIntegration,
    AIIntegration,
    IntegrationHealthCheck,
    bootstrap_integration
)

# ===== FIXTURES =====

@pytest.fixture
def integration_manager():
    """Create fresh integration manager"""
    return IntegrationManager()

@pytest.fixture
def bootstrapped_manager():
    """Create bootstrapped manager with all components registered"""
    return bootstrap_integration(marketplace=True, debugger=True, ai=True)

# ===== INTEGRATION MANAGER TESTS =====

class TestIntegrationManager:
    """Test IntegrationManager core functionality"""
    
    def test_register_component(self, integration_manager):
        """Test component registration"""
        metadata = ComponentMetadata(
            name="Test Component",
            type=ComponentType.MARKETPLACE
        )
        
        result = integration_manager.register_component("test_comp", metadata)
        assert result is True
        assert "test_comp" in integration_manager.components
    
    def test_register_duplicate_component(self, integration_manager):
        """Test that duplicate registration returns False"""
        metadata = ComponentMetadata(
            name="Test Component",
            type=ComponentType.MARKETPLACE
        )
        
        integration_manager.register_component("test_comp", metadata)
        result = integration_manager.register_component("test_comp", metadata)
        
        assert result is False
    
    def test_initialize_component(self, integration_manager):
        """Test component initialization"""
        metadata = ComponentMetadata(
            name="Test Component",
            type=ComponentType.MARKETPLACE,
            status=IntegrationStatus.UNINITIALIZED
        )
        integration_manager.register_component("test_comp", metadata)
        
        # Mock initializer
        initializer = Mock(return_value=None)
        
        result = integration_manager.initialize_component("test_comp", initializer)
        
        assert result is True
        assert integration_manager.components["test_comp"].status == IntegrationStatus.READY
        initializer.assert_called_once()
    
    def test_initialize_nonexistent_component(self, integration_manager):
        """Test initializing nonexistent component"""
        initializer = Mock()
        result = integration_manager.initialize_component("nonexistent", initializer)
        
        assert result is False
    
    def test_get_component_status(self, integration_manager):
        """Test getting component status"""
        metadata = ComponentMetadata(
            name="Test Component",
            type=ComponentType.MARKETPLACE
        )
        integration_manager.register_component("test_comp", metadata)
        
        status = integration_manager.get_component_status("test_comp")
        assert status is not None
        assert status.name == "Test Component"
    
    def test_record_and_retrieve_metrics(self, integration_manager):
        """Test recording and retrieving metrics"""
        metric = PerformanceMetric(
            component="test",
            metric_name="latency",
            value=100.5,
            unit="ms"
        )
        
        integration_manager.record_metric(metric)
        
        metrics = integration_manager.get_metrics(component="test")
        assert len(metrics) == 1
        assert metrics[0].value == 100.5
    
    def test_event_subscription(self, integration_manager):
        """Test event subscription and firing"""
        handler = Mock()
        
        event_key = integration_manager.subscribe_to_events(
            ComponentType.MARKETPLACE,
            "initialized",
            handler
        )
        
        # Fire event
        event = IntegrationEvent(
            component=ComponentType.MARKETPLACE,
            event_type="initialized"
        )
        integration_manager._fire_event(event)
        
        handler.assert_called_once()

# ===== MARKETPLACE INTEGRATION TESTS =====

class TestMarketplaceIntegration:
    """Test Marketplace integration"""
    
    @patch('integration.integration_manager.MarketplaceService')
    @patch('integration.integration_manager.InstallationService')
    def test_marketplace_initialization(self, mock_install, mock_marketplace, integration_manager):
        """Test marketplace initialization"""
        marketplace = MarketplaceIntegration(integration_manager)
        marketplace.initialize()
        
        assert marketplace.marketplace_service is not None
        assert marketplace.installation_service is not None
    
    @patch('integration.integration_manager.MarketplaceService')
    def test_search_plugins(self, mock_marketplace_class, integration_manager):
        """Test plugin search"""
        # Setup mock
        mock_service = Mock()
        mock_marketplace_class.return_value = mock_service
        
        mock_plugin = Mock()
        mock_plugin.id = "plugin1"
        mock_plugin.name = "Test Plugin"
        mock_plugin.version = "1.0.0"
        mock_plugin.rating = 4.5
        mock_plugin.downloads = 100
        mock_plugin.description = "A test plugin"
        
        mock_service.search_plugins.return_value = [mock_plugin]
        
        marketplace = MarketplaceIntegration(integration_manager)
        marketplace.initialize()
        
        results = marketplace.search_plugins("test", limit=10)
        
        assert len(results) > 0
        assert results[0]["name"] == "Test Plugin"
        assert results[0]["rating"] == 4.5

# ===== DEBUGGER INTEGRATION TESTS =====

class TestDebuggerIntegration:
    """Test Debugger integration"""
    
    @patch('integration.integration_manager.DebuggerEngine')
    @patch('integration.integration_manager.DebugConsole')
    @patch('integration.integration_manager.PerformanceProfiler')
    def test_debugger_initialization(self, mock_profiler, mock_console, mock_engine, integration_manager):
        """Test debugger initialization"""
        debugger = DebuggerIntegration(integration_manager)
        debugger.initialize()
        
        assert debugger.debugger_engine is not None
        assert debugger.debug_console is not None
        assert debugger.profiler is not None
    
    @patch('integration.integration_manager.DebuggerEngine')
    def test_create_breakpoint(self, mock_engine_class, integration_manager):
        """Test breakpoint creation"""
        mock_engine = Mock()
        mock_engine_class.return_value = mock_engine
        mock_engine.create_breakpoint.return_value = "bp_123"
        
        debugger = DebuggerIntegration(integration_manager)
        debugger.initialize()
        
        bp_id = debugger.create_breakpoint("test.bas", 10, condition="x > 5")
        
        assert bp_id == "bp_123"
        mock_engine.create_breakpoint.assert_called_once()

# ===== AI INTEGRATION TESTS =====

class TestAIIntegration:
    """Test AI integration"""
    
    @patch('integration.integration_manager.CodeCompletionEngine')
    @patch('integration.integration_manager.BugDetectionEngine')
    @patch('integration.integration_manager.ReviewInsightEngine')
    @patch('integration.integration_manager.LearningPathGenerator')
    @patch('integration.integration_manager.PerformanceOptimizationAdvisor')
    def test_ai_initialization(self, mock_opt, mock_learn, mock_review, 
                               mock_bug, mock_completion, integration_manager):
        """Test AI initialization"""
        ai = AIIntegration(integration_manager)
        ai.initialize()
        
        assert ai.completion_engine is not None
        assert ai.bug_detector is not None
        assert ai.review_engine is not None
        assert ai.learning_generator is not None
        assert ai.optimizer is not None
    
    @patch('integration.integration_manager.CodeCompletionEngine')
    def test_get_code_completions(self, mock_completion_class, integration_manager):
        """Test code completion"""
        mock_engine = Mock()
        mock_completion_class.return_value = mock_engine
        
        mock_suggestion = Mock()
        mock_suggestion.suggested_code = "IF x > 10 THEN"
        mock_suggestion.description = "Conditional statement"
        mock_suggestion.confidence = 0.95
        
        mock_engine.suggest_completion.return_value = [mock_suggestion]
        
        ai = AIIntegration(integration_manager)
        ai.initialize()
        
        completions = ai.get_code_completions("IF", 0, "basic")
        
        assert len(completions) > 0
        assert completions[0]["confidence"] == 0.95
    
    @patch('integration.integration_manager.BugDetectionEngine')
    def test_detect_bugs(self, mock_bug_class, integration_manager):
        """Test bug detection"""
        mock_engine = Mock()
        mock_bug_class.return_value = mock_engine
        
        mock_bug = Mock()
        mock_bug.line = 5
        mock_bug.severity.value = "warning"
        mock_bug.message = "Variable may be uninitialized"
        mock_bug.suggested_fix = "Initialize variable before use"
        
        mock_engine.analyze_code.return_value = [mock_bug]
        
        ai = AIIntegration(integration_manager)
        ai.initialize()
        
        bugs = ai.detect_bugs("IF x > 10 THEN", "basic")
        
        assert len(bugs) > 0
        assert bugs[0]["message"] == "Variable may be uninitialized"

# ===== HEALTH CHECK TESTS =====

class TestIntegrationHealthCheck:
    """Test health check functionality"""
    
    def test_health_check(self, bootstrapped_manager):
        """Test health check"""
        health = IntegrationHealthCheck(bootstrapped_manager)
        status = health.perform_health_check()
        
        assert "timestamp" in status
        assert "components" in status
        assert "metrics" in status
        assert "overall_status" in status
    
    def test_health_check_with_error(self, integration_manager):
        """Test health check with component error"""
        metadata = ComponentMetadata(
            name="Failed Component",
            type=ComponentType.MARKETPLACE,
            status=IntegrationStatus.ERROR,
            error_message="Initialization failed"
        )
        integration_manager.register_component("failed", metadata)
        
        health = IntegrationHealthCheck(integration_manager)
        status = health.perform_health_check()
        
        assert status["overall_status"] == "degraded"
        assert status["components"]["failed"]["error"] == "Initialization failed"

# ===== END-TO-END WORKFLOW TESTS =====

class TestEndToEndWorkflows:
    """Test complete workflows"""
    
    def test_marketplace_workflow(self, bootstrapped_manager):
        """Test: Register → Initialize → Use marketplace"""
        marketplace = MarketplaceIntegration(bootstrapped_manager)
        
        # Register marketplace
        bootstrapped_manager.register_component(
            "marketplace_e2e",
            ComponentMetadata(
                name="Marketplace E2E",
                type=ComponentType.MARKETPLACE
            )
        )
        
        # Initialize
        result = bootstrapped_manager.initialize_component(
            "marketplace_e2e",
            marketplace.initialize
        )
        
        assert result is True
        
        # Verify status
        status = bootstrapped_manager.get_component_status("marketplace_e2e")
        assert status.status in [IntegrationStatus.READY, IntegrationStatus.ERROR]
    
    def test_debugger_workflow(self, bootstrapped_manager):
        """Test: Register → Initialize → Debug"""
        debugger = DebuggerIntegration(bootstrapped_manager)
        
        bootstrapped_manager.register_component(
            "debugger_e2e",
            ComponentMetadata(
                name="Debugger E2E",
                type=ComponentType.DEBUGGER
            )
        )
        
        result = bootstrapped_manager.initialize_component(
            "debugger_e2e",
            debugger.initialize
        )
        
        assert result is True
    
    def test_ai_workflow(self, bootstrapped_manager):
        """Test: Register → Initialize → Analyze"""
        ai = AIIntegration(bootstrapped_manager)
        
        bootstrapped_manager.register_component(
            "ai_e2e",
            ComponentMetadata(
                name="AI E2E",
                type=ComponentType.AI_INTELLIGENCE
            )
        )
        
        result = bootstrapped_manager.initialize_component(
            "ai_e2e",
            ai.initialize
        )
        
        assert result is True

# ===== PERFORMANCE TESTS =====

class TestPerformanceBenchmarks:
    """Test performance characteristics"""
    
    def test_component_registration_performance(self, integration_manager):
        """Test registration performance"""
        import time
        
        start = time.time()
        
        for i in range(100):
            metadata = ComponentMetadata(
                name=f"Component {i}",
                type=ComponentType.MARKETPLACE
            )
            integration_manager.register_component(f"comp_{i}", metadata)
        
        elapsed = time.time() - start
        
        # Should complete quickly
        assert elapsed < 1.0  # 100 registrations in < 1 second
    
    def test_metric_recording_performance(self, integration_manager):
        """Test metric recording performance"""
        import time
        
        start = time.time()
        
        for i in range(1000):
            metric = PerformanceMetric(
                component="test",
                metric_name="latency",
                value=float(i),
                unit="ms"
            )
            integration_manager.record_metric(metric)
        
        elapsed = time.time() - start
        
        # Should be fast
        assert elapsed < 0.5  # 1000 metrics in < 500ms
    
    def test_event_dispatch_performance(self, integration_manager):
        """Test event dispatch performance"""
        import time
        
        handler = Mock()
        event_key = integration_manager.subscribe_to_events(
            ComponentType.MARKETPLACE,
            "test",
            handler
        )
        
        start = time.time()
        
        for i in range(100):
            event = IntegrationEvent(
                component=ComponentType.MARKETPLACE,
                event_type="test"
            )
            integration_manager._fire_event(event)
        
        elapsed = time.time() - start
        
        # Should be fast
        assert elapsed < 0.1  # 100 events in < 100ms

# ===== CONCURRENCY TESTS =====

class TestConcurrency:
    """Test thread safety"""
    
    def test_concurrent_component_registration(self, integration_manager):
        """Test thread-safe registration"""
        import threading
        
        errors = []
        
        def register_components():
            try:
                for i in range(10):
                    metadata = ComponentMetadata(
                        name=f"Component {i}",
                        type=ComponentType.MARKETPLACE
                    )
                    integration_manager.register_component(
                        f"comp_{threading.current_thread().ident}_{i}",
                        metadata
                    )
            except Exception as e:
                errors.append(e)
        
        threads = [threading.Thread(target=register_components) for _ in range(5)]
        
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        
        assert len(errors) == 0
        assert len(integration_manager.components) == 50

if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
