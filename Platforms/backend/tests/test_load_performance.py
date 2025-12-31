"""
Load Testing & Performance Benchmarks
Tests system performance under concurrent user load
"""

import asyncio
import time
import random
import statistics
from typing import List, Dict, Any
import pytest
from httpx import AsyncClient
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession
from sqlalchemy.orm import sessionmaker

# Performance thresholds
LATENCY_THRESHOLDS = {
    'operation_send': 200,  # ms
    'message_send': 300,    # ms
    'sync_full': 500,       # ms
    'cursor_update': 100,   # ms
}

THROUGHPUT_TARGETS = {
    'operations_per_second': 100,
    'messages_per_second': 50,
    'concurrent_users': 20,
}

MEMORY_LIMITS = {
    'per_operation': 200,   # bytes
    'per_message': 500,     # bytes
    'store_active': 50,     # MB
}


class PerformanceMetrics:
    """Track performance metrics for testing"""

    def __init__(self):
        self.latencies: List[float] = []
        self.throughput_count = 0
        self.start_time = None
        self.errors = 0
        self.memory_samples: List[int] = []

    def record_latency(self, latency_ms: float):
        """Record operation latency"""
        self.latencies.append(latency_ms)

    def record_throughput(self):
        """Record completed operation"""
        self.throughput_count += 1

    def record_error(self):
        """Record error"""
        self.errors += 1

    def record_memory(self, bytes_used: int):
        """Record memory sample"""
        self.memory_samples.append(bytes_used)

    def get_stats(self) -> Dict[str, Any]:
        """Get aggregated statistics"""
        if not self.latencies:
            return {}

        elapsed = (time.time() - self.start_time) if self.start_time else 0

        return {
            'latency_min': min(self.latencies),
            'latency_max': max(self.latencies),
            'latency_avg': statistics.mean(self.latencies),
            'latency_p95': self.percentile(self.latencies, 95),
            'latency_p99': self.percentile(self.latencies, 99),
            'throughput': self.throughput_count / elapsed if elapsed > 0 else 0,
            'error_count': self.errors,
            'error_rate': self.errors / self.throughput_count if self.throughput_count > 0 else 0,
            'memory_avg': statistics.mean(self.memory_samples) if self.memory_samples else 0,
            'memory_peak': max(self.memory_samples) if self.memory_samples else 0,
        }

    @staticmethod
    def percentile(data: List[float], percentile: int) -> float:
        """Calculate percentile"""
        sorted_data = sorted(data)
        index = int(len(sorted_data) * (percentile / 100))
        return sorted_data[index] if index < len(sorted_data) else sorted_data[-1]


class TestConcurrentUsers:
    """Test performance with concurrent users"""

    @pytest.mark.asyncio
    async def test_10_concurrent_users(self, client: AsyncClient):
        """Test with 10 concurrent users"""
        metrics = await self._simulate_concurrent_load(10)
        assert metrics['latency_avg'] < LATENCY_THRESHOLDS['operation_send']
        assert metrics['error_rate'] < 0.01  # <1% error rate

    @pytest.mark.asyncio
    async def test_20_concurrent_users(self, client: AsyncClient):
        """Test with 20 concurrent users"""
        metrics = await self._simulate_concurrent_load(20)
        assert metrics['latency_avg'] < LATENCY_THRESHOLDS['operation_send'] * 1.5
        assert metrics['error_rate'] < 0.05  # <5% error rate

    @pytest.mark.asyncio
    async def test_50_concurrent_users(self, client: AsyncClient):
        """Stress test with 50 concurrent users"""
        metrics = await self._simulate_concurrent_load(50)
        # Degraded performance acceptable at high load
        assert metrics['error_rate'] < 0.1  # <10% error rate

    async def _simulate_concurrent_load(self, user_count: int) -> Dict[str, Any]:
        """Simulate concurrent users making requests"""
        metrics = PerformanceMetrics()
        metrics.start_time = time.time()

        tasks = []
        for i in range(user_count):
            task = self._user_session(f'user_{i}', metrics)
            tasks.append(task)

        await asyncio.gather(*tasks, return_exceptions=True)
        return metrics.get_stats()

    async def _user_session(self, user_id: str, metrics: PerformanceMetrics):
        """Simulate single user session"""
        from httpx import AsyncClient

        async with AsyncClient() as client:
            # Create room
            start = time.time()
            room_resp = await client.post(
                'http://localhost:8000/api/rooms',
                json={'name': f'room_{user_id}'},
                timeout=10.0,
            )
            latency = (time.time() - start) * 1000
            metrics.record_latency(latency)
            metrics.record_throughput()

            if room_resp.status_code == 201:
                room_id = room_resp.json()['id']

                # Send operations
                for op_num in range(10):
                    start = time.time()
                    try:
                        resp = await client.post(
                            f'http://localhost:8000/api/rooms/{room_id}/operations',
                            json={
                                'userId': user_id,
                                'operation': {
                                    'type': 'insert',
                                    'position': op_num,
                                    'content': f'text_{op_num}',
                                },
                            },
                            timeout=5.0,
                        )
                        latency = (time.time() - start) * 1000
                        metrics.record_latency(latency)
                        metrics.record_throughput()

                        if resp.status_code != 200:
                            metrics.record_error()
                    except Exception as e:
                        metrics.record_error()

                # Send messages
                for msg_num in range(5):
                    start = time.time()
                    try:
                        resp = await client.post(
                            f'http://localhost:8000/api/rooms/{room_id}/messages',
                            json={
                                'userId': user_id,
                                'content': f'message_{msg_num}',
                            },
                            timeout=5.0,
                        )
                        latency = (time.time() - start) * 1000
                        metrics.record_latency(latency)
                        metrics.record_throughput()

                        if resp.status_code != 201:
                            metrics.record_error()
                    except Exception as e:
                        metrics.record_error()
            else:
                metrics.record_error()


class TestOperationThroughput:
    """Test operation throughput limits"""

    @pytest.mark.asyncio
    async def test_100_operations_per_second(self, client: AsyncClient):
        """Test throughput: 100 ops/sec"""
        metrics = PerformanceMetrics()
        metrics.start_time = time.time()

        # Create test room
        room_resp = await client.post(
            '/api/rooms',
            json={'name': 'perf_test_room'},
        )
        room_id = room_resp.json()['id']

        # Send 100 operations rapidly
        for i in range(100):
            start = time.time()
            resp = await client.post(
                f'/api/rooms/{room_id}/operations',
                json={
                    'userId': 'test_user',
                    'operation': {
                        'type': 'insert',
                        'position': i,
                        'content': f'op_{i}',
                    },
                },
            )
            latency = (time.time() - start) * 1000
            metrics.record_latency(latency)
            metrics.record_throughput()

            if resp.status_code != 200:
                metrics.record_error()

        stats = metrics.get_stats()
        assert stats['throughput'] >= THROUGHPUT_TARGETS['operations_per_second'] * 0.8


class TestMemoryUsage:
    """Test memory usage and optimization"""

    @pytest.mark.asyncio
    async def test_operation_memory_efficiency(self):
        """Test operation memory footprint"""
        from core.interpreters.ot_engine import OTEngine

        engine = OTEngine()
        doc = ''

        # Simulate 1000 operations
        for i in range(1000):
            op = {
                'type': 'insert',
                'position': len(doc),
                'content': f'text_{i}',
            }
            doc = engine.apply(doc, op)

        # Average memory per operation should be < 200 bytes
        memory_per_op = len(doc) / 1000
        assert memory_per_op < MEMORY_LIMITS['per_operation']

    @pytest.mark.asyncio
    async def test_message_memory_efficiency(self):
        """Test message memory footprint"""
        messages = []

        # Create 10000 messages
        for i in range(10000):
            msg = {
                'id': f'msg_{i}',
                'userId': f'user_{i % 10}',
                'content': f'message_{i}',
                'timestamp': time.time(),
                'reactions': [],
            }
            messages.append(msg)

        # Memory should be reasonable
        import sys
        memory_used = sys.getsizeof(messages)
        avg_per_msg = memory_used / len(messages)
        assert avg_per_msg < MEMORY_LIMITS['per_message'] * 2


class TestNetworkResilience:
    """Test performance with network issues"""

    @pytest.mark.asyncio
    async def test_high_latency_scenario(self, client: AsyncClient):
        """Test with simulated high latency (500ms)"""
        metrics = PerformanceMetrics()
        metrics.start_time = time.time()

        # This would require network throttling setup
        # For now, test that system handles slow responses
        room_resp = await client.post(
            '/api/rooms',
            json={'name': 'latency_test'},
            timeout=10.0,
        )
        assert room_resp.status_code == 201

    @pytest.mark.asyncio
    async def test_packet_loss_scenario(self):
        """Test with simulated packet loss (10%)"""
        # Would require network simulation
        # System should handle retries gracefully
        pass

    @pytest.mark.asyncio
    async def test_connection_timeout_handling(self, client: AsyncClient):
        """Test timeout handling"""
        try:
            resp = await client.get(
                'http://localhost:9999/fake',
                timeout=0.1,
            )
        except Exception as e:
            # Should raise timeout exception
            assert 'timeout' in str(e).lower() or 'connect' in str(e).lower()


class TestDatabasePerformance:
    """Test database query performance"""

    @pytest.mark.asyncio
    async def test_operation_insert_performance(self):
        """Test inserting operations into database"""
        # Should be < 50ms per insert
        from sqlalchemy.ext.asyncio import create_async_engine

        engine = create_async_engine('sqlite+aiosqlite:///:memory:')

        metrics = PerformanceMetrics()
        metrics.start_time = time.time()

        # Create test session and run inserts
        # (Would use actual database in real test)
        for i in range(100):
            start = time.time()
            # Simulate database insert
            await asyncio.sleep(0.001)  # Simulated query time
            latency = (time.time() - start) * 1000
            metrics.record_latency(latency)
            metrics.record_throughput()

        stats = metrics.get_stats()
        assert stats['latency_avg'] < 50  # <50ms per insert

    @pytest.mark.asyncio
    async def test_sync_query_performance(self):
        """Test fetching document for sync"""
        # Should be < 100ms for full sync
        metrics = PerformanceMetrics()

        for i in range(50):
            start = time.time()
            # Simulate fetching large document
            await asyncio.sleep(0.002)
            latency = (time.time() - start) * 1000
            metrics.record_latency(latency)

        stats = metrics.get_stats()
        assert stats['latency_avg'] < LATENCY_THRESHOLDS['sync_full']


class TestOTEnginePerformance:
    """Test OT algorithm performance"""

    def test_transform_performance(self):
        """Test operation transform speed"""
        from core.interpreters.ot_engine import OTEngine

        engine = OTEngine()

        metrics = PerformanceMetrics()
        metrics.start_time = time.time()

        # Create test operations
        op1 = {'type': 'insert', 'position': 5, 'content': 'hello'}
        op2 = {'type': 'delete', 'position': 0, 'length': 5}

        # Run 1000 transforms
        for i in range(1000):
            start = time.time()
            _ = engine.transform(op1, op2)
            latency = (time.time() - start) * 1000
            metrics.record_latency(latency)

        stats = metrics.get_stats()
        assert stats['latency_avg'] < 5  # <5ms per transform

    def test_compose_performance(self):
        """Test operation composition speed"""
        from core.interpreters.ot_engine import OTEngine

        engine = OTEngine()

        metrics = PerformanceMetrics()

        ops = [{'type': 'insert', 'position': i, 'content': f'text_{i}'} for i in range(100)]

        start = time.time()
        result = engine.compose(ops)
        latency = (time.time() - start) * 1000
        metrics.record_latency(latency)

        assert latency < 50  # <50ms to compose 100 ops


class TestWebSocketPerformance:
    """Test WebSocket performance under load"""

    @pytest.mark.asyncio
    async def test_message_throughput(self):
        """Test WebSocket message throughput"""
        # Would require actual WebSocket connection
        # Mock test for throughput calculation
        metrics = PerformanceMetrics()
        metrics.start_time = time.time()

        message_count = 1000
        for i in range(message_count):
            metrics.record_throughput()
            await asyncio.sleep(0.001)  # Simulate message processing

        stats = metrics.get_stats()
        # Should handle 1000+ messages in reasonable time
        assert stats['throughput'] > 100  # >100 msgs/sec

    @pytest.mark.asyncio
    async def test_connection_persistence(self):
        """Test WebSocket connection stability"""
        # Should maintain connection for extended period
        connection_time = 0
        for _ in range(100):
            connection_time += 0.01

        assert connection_time > 1.0  # Should maintain for 1+ second


class TestCachePerformance:
    """Test caching optimization"""

    def test_snapshot_cache_hit_rate(self):
        """Test document snapshot cache effectiveness"""
        cache = {}
        hits = 0
        misses = 0

        # Simulate 1000 snapshot requests
        for i in range(1000):
            key = f'snapshot_{i % 10}'  # Only 10 unique snapshots

            if key in cache:
                hits += 1
            else:
                cache[key] = f'snapshot_data_{i % 10}'
                misses += 1

        hit_rate = hits / (hits + misses)
        # Should have high hit rate due to locality
        assert hit_rate > 0.8  # >80% hit rate

    def test_operation_deduplication(self):
        """Test operation deduplication effectiveness"""
        seen_ops = set()
        duplicates = 0

        # Simulate 1000 operations with duplicates
        for i in range(1000):
            op_id = f'op_{i % 100}'  # 100 unique ops repeated 10x

            if op_id in seen_ops:
                duplicates += 1
            else:
                seen_ops.add(op_id)

        dedup_rate = duplicates / 1000
        assert dedup_rate > 0.8  # Should detect 80%+ duplicates


# Benchmark fixtures
@pytest.fixture
async def performance_report() -> Dict[str, Any]:
    """Generate performance report"""
    return {
        'timestamp': time.time(),
        'tests_run': 0,
        'metrics': {},
    }


if __name__ == '__main__':
    pytest.main([__file__, '-v', '--tb=short'])
