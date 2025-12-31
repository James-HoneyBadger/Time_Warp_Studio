/**
 * Comprehensive Integration & E2E Test Suite
 * 
 * Tests complete workflows across all system components
 */

describe('Time Warp Studio - Complete Integration Tests', () => {
  
  // ===== ENVIRONMENT SETUP =====
  
  let api: ApiClient;
  let wsClient: WebSocketClient;
  let testUser: TestUser;
  
  beforeAll(async () => {
    api = new ApiClient(process.env.API_URL || 'http://localhost:8000');
    testUser = await api.createTestUser('integration-test-user');
    wsClient = new WebSocketClient(`${process.env.WS_URL || 'ws://localhost:8000'}/ws?token=${testUser.token}`);
    await wsClient.connect();
  });
  
  afterAll(async () => {
    await wsClient.disconnect();
    await api.deleteTestUser(testUser.id);
  });

  // ===== MULTIPLAYER COLLABORATION TESTS =====
  
  describe('Real-time Collaboration', () => {
    test('should sync code changes across multiple clients', async () => {
      const session = await api.createSession('test-collaboration');
      const code = 'PRINT "Hello, World!"';
      
      // Client 1 makes change
      await api.updateCode(session.id, code);
      
      // Client 2 receives update via WebSocket
      const update = await wsClient.waitForMessage('code-update', 5000);
      expect(update.code).toBe(code);
    });

    test('should handle concurrent edits with conflict resolution', async () => {
      const session = await api.createSession('test-conflicts');
      
      // Two clients edit simultaneously
      const promise1 = api.updateCode(session.id, 'version1');
      const promise2 = api.updateCode(session.id, 'version2');
      
      const [result1, result2] = await Promise.all([promise1, promise2]);
      
      // Should merge or resolve conflicts
      expect(result1.version || result2.version).toBeDefined();
    });

    test('should broadcast cursor positions to collaborators', async () => {
      const session = await api.createSession('test-cursors');
      
      await api.setCursorPosition(session.id, { line: 5, column: 10 });
      const cursorUpdate = await wsClient.waitForMessage('cursor-update', 5000);
      
      expect(cursorUpdate.line).toBe(5);
      expect(cursorUpdate.column).toBe(10);
    });

    test('should maintain user presence tracking', async () => {
      const session = await api.createSession('test-presence');
      
      const users = await api.getActiveUsers(session.id);
      expect(users.length).toBeGreaterThan(0);
      expect(users[0]).toHaveProperty('id');
      expect(users[0]).toHaveProperty('lastActive');
    });

    test('should handle user disconnection gracefully', async () => {
      const session = await api.createSession('test-disconnect');
      const user2 = await api.createTestUser('disconnect-user');
      
      await wsClient.disconnect();
      
      const users = await api.getActiveUsers(session.id);
      expect(users.find(u => u.id === testUser.id)).toBeUndefined();
    });
  });

  // ===== LANGUAGE EXECUTION TESTS =====
  
  describe('WASM Language Execution', () => {
    test('should execute BASIC code with correct output', async () => {
      const code = 'PRINT "Hello"\nPRINT 2 + 2';
      const result = await api.executeCode('basic', code);
      
      expect(result.success).toBe(true);
      expect(result.output).toContain('Hello');
      expect(result.output).toContain('4');
      expect(result.stats.duration).toBeLessThan(50);
    });

    test('should execute Logo with turtle graphics', async () => {
      const code = 'REPEAT 4 [FORWARD 100 RIGHT 90]';
      const result = await api.executeCode('logo', code);
      
      expect(result.success).toBe(true);
      expect(result.graphics).toBeDefined();
      expect(result.graphics.length).toBeGreaterThan(0);
    });

    test('should execute Pascal with type system', async () => {
      const code = `
        VAR x: INTEGER;
        BEGIN
          x := 10;
          WRITELN(x * 2)
        END
      `;
      const result = await api.executeCode('pascal', code);
      
      expect(result.success).toBe(true);
      expect(result.output).toContain('20');
    });

    test('should execute Prolog with unification', async () => {
      const code = `
        parent(tom, bob).
        parent(tom, liz).
        sibling(X, Y) :- parent(Z, X), parent(Z, Y).
        ?- sibling(bob, liz).
      `;
      const result = await api.executeCode('prolog', code);
      
      expect(result.success).toBe(true);
    });

    test('should handle syntax errors appropriately', async () => {
      const code = 'PRINT "unclosed string';
      const result = await api.executeCode('basic', code);
      
      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
      expect(result.error.code).toBe('ERR_SYNTAX');
      expect(result.error.line).toBeGreaterThanOrEqual(0);
    });

    test('should enforce execution timeout', async () => {
      const code = 'WHILE 1 DO PRINT "infinite"';
      const result = await api.executeCode('basic', code, { timeout: 1000 });
      
      expect(result.success).toBe(false);
      expect(result.error.code).toBe('ERR_TIMEOUT');
    });

    test('should fallback to server execution if WASM fails', async () => {
      const code = 'PRINT "Hello"';
      const result = await api.executeCode('basic', code);
      
      // Should succeed via WASM or fallback
      expect(result.success).toBe(true);
      expect(result.stats.executionMode).toMatch(/wasm|fallback/);
    });
  });

  // ===== GRAPHICS RENDERING TESTS =====
  
  describe('Graphics & Turtle Graphics', () => {
    test('should render turtle graphics commands', async () => {
      const code = `
        FORWARD 50
        RIGHT 90
        FORWARD 50
        HOME
        PENUP
        FORWARD 100
        PENDOWN
      `;
      const result = await api.executeCode('logo', code);
      
      expect(result.graphics).toBeDefined();
      const commands = result.graphics;
      expect(commands.some(c => c.type === 'TURTLE_MOVE')).toBe(true);
      expect(commands.some(c => c.type === 'TURTLE_ROTATE')).toBe(true);
      expect(commands.some(c => c.type === 'TURTLE_PEN_UP')).toBe(true);
    });

    test('should handle color changes', async () => {
      const code = 'SETCOLOR "red"\nFORWARD 50';
      const result = await api.executeCode('logo', code);
      
      expect(result.graphics.some(c => c.type === 'TURTLE_SET_COLOR')).toBe(true);
    });

    test('should clear canvas correctly', async () => {
      const code = 'HOME\nCLEARSCREEN';
      const result = await api.executeCode('logo', code);
      
      expect(result.graphics.some(c => c.type === 'CLEAR_SCREEN')).toBe(true);
    });

    test('should support multiple canvas instances', async () => {
      const canvas1 = await api.createCanvas('canvas1');
      const canvas2 = await api.createCanvas('canvas2');
      
      await api.renderGraphics(canvas1.id, [
        { type: 'TURTLE_MOVE', distance: 50 }
      ]);
      
      await api.renderGraphics(canvas2.id, [
        { type: 'TURTLE_MOVE', distance: 100 }
      ]);
      
      const canvases = await api.getCanvases();
      expect(canvases.length).toBeGreaterThanOrEqual(2);
    });
  });

  // ===== DEBUGGING TESTS =====
  
  describe('Debugging Infrastructure', () => {
    test('should set and hit breakpoints', async () => {
      const code = 'x = 10\ny = 20\nz = x + y';
      await api.setBreakpoint('test-debug', 2);
      
      const result = await api.executeCodeWithDebugger('basic', code);
      expect(result.debugInfo.breakpointHit).toBe(true);
      expect(result.debugInfo.line).toBe(2);
    });

    test('should track watch variables', async () => {
      const code = 'x = 10\nx = 20\nPRINT x';
      await api.addWatch('test-debug', 'x');
      
      const result = await api.executeCodeWithDebugger('basic', code);
      expect(result.debugInfo.watches).toBeDefined();
      expect(result.debugInfo.watches['x']).toBeDefined();
    });

    test('should record execution traces', async () => {
      const code = 'PRINT "step1"\nPRINT "step2"';
      const result = await api.executeCodeWithDebugger('basic', code);
      
      expect(result.debugInfo.traces).toBeDefined();
      expect(result.debugInfo.traces.length).toBeGreaterThan(0);
    });

    test('should profile function performance', async () => {
      const code = `
        FUNCTION slow()
          FOR i = 1 TO 1000
            x = i * 2
          NEXT i
        END FUNCTION
        CALL slow()
      `;
      const result = await api.executeCodeWithDebugger('basic', code);
      
      expect(result.debugInfo.profiling).toBeDefined();
      expect(result.debugInfo.profiling.slow).toBeDefined();
      expect(result.debugInfo.profiling.slow.calls).toBe(1);
      expect(result.debugInfo.profiling.slow.totalTime).toBeGreaterThan(0);
    });

    test('should handle conditional breakpoints', async () => {
      const code = `
        FOR i = 1 TO 100
          PRINT i
        NEXT i
      `;
      await api.setConditionalBreakpoint('test-debug', 2, 'i > 50');
      
      const result = await api.executeCodeWithDebugger('basic', code);
      expect(result.debugInfo.breakpointHit).toBe(true);
      expect(result.debugInfo.variables.i).toBeGreaterThan(50);
    });
  });

  // ===== PERFORMANCE TESTS =====
  
  describe('Performance & Optimization', () => {
    test('should execute code with minimal latency', async () => {
      const code = 'PRINT "Hello"';
      const result = await api.executeCode('basic', code);
      
      expect(result.stats.duration).toBeLessThan(50);
    });

    test('should handle large code files efficiently', async () => {
      // Generate 1000 line code
      let code = '';
      for (let i = 0; i < 1000; i++) {
        code += `PRINT "${i}"\n`;
      }
      
      const result = await api.executeCode('basic', code);
      expect(result.stats.duration).toBeLessThan(500);
      expect(result.success).toBe(true);
    });

    test('should use zero-copy for large data transfers', async () => {
      const largeData = new Uint8Array(1000000);
      const result = await api.sendZeroCopyData('test-buffer', largeData);
      
      expect(result.transferred).toBe(1000000);
      expect(result.copyCount).toBe(1); // Only copy for transport
    });

    test('should pool memory efficiently', async () => {
      const initialMemory = performance.memory?.usedJSHeapSize || 0;
      
      for (let i = 0; i < 100; i++) {
        await api.executeCode('basic', 'PRINT "test"');
      }
      
      const finalMemory = performance.memory?.usedJSHeapSize || 0;
      const memoryGrowth = finalMemory - initialMemory;
      
      // Memory growth should be minimal due to pooling
      expect(memoryGrowth).toBeLessThan(5000000); // 5MB
    });

    test('should lazy load modules on demand', async () => {
      const startTime = performance.now();
      const result = await api.executeCode('basic', 'PRINT "hello"');
      const loadTime = performance.now() - startTime;
      
      // First execution loads module
      expect(loadTime).toBeLessThan(200);
      
      // Second execution uses cached module
      const startTime2 = performance.now();
      await api.executeCode('basic', 'PRINT "world"');
      const cachedLoadTime = performance.now() - startTime2;
      
      expect(cachedLoadTime).toBeLessThan(loadTime);
    });

    test('should monitor bundle size', async () => {
      const bundleSize = await api.getBundleSize();
      
      expect(bundleSize).toBeLessThan(250000); // 250KB
      expect(bundleSize).toBeGreaterThan(100000); // 100KB
    });
  });

  // ===== LOAD TESTING =====
  
  describe('Load & Stress Testing', () => {
    test('should handle multiple concurrent executions', async () => {
      const promises = [];
      for (let i = 0; i < 50; i++) {
        promises.push(api.executeCode('basic', `PRINT "${i}"`));
      }
      
      const results = await Promise.all(promises);
      const successCount = results.filter(r => r.success).length;
      
      expect(successCount).toBe(50);
    });

    test('should handle multiple concurrent connections', async () => {
      const clients = [];
      for (let i = 0; i < 10; i++) {
        const client = new WebSocketClient(`${process.env.WS_URL || 'ws://localhost:8000'}/ws?token=${testUser.token}`);
        await client.connect();
        clients.push(client);
      }
      
      // All clients should be connected
      const connectedCount = clients.filter(c => c.isConnected).length;
      expect(connectedCount).toBe(10);
      
      // Clean up
      await Promise.all(clients.map(c => c.disconnect()));
    });

    test('should maintain performance under sustained load', async () => {
      const durations: number[] = [];
      
      for (let i = 0; i < 100; i++) {
        const start = performance.now();
        await api.executeCode('basic', 'PRINT "load test"');
        const duration = performance.now() - start;
        durations.push(duration);
      }
      
      const avgDuration = durations.reduce((a, b) => a + b) / durations.length;
      const p95Duration = durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.95)];
      
      expect(avgDuration).toBeLessThan(100);
      expect(p95Duration).toBeLessThan(150);
    });
  });

  // ===== ERROR RECOVERY TESTS =====
  
  describe('Error Handling & Recovery', () => {
    test('should recover from database connection loss', async () => {
      // Simulate connection loss
      await api.disconnectDatabase();
      
      // Should eventually recover
      let success = false;
      for (let i = 0; i < 10; i++) {
        try {
          await api.getSystemStatus();
          success = true;
          break;
        } catch (e) {
          await new Promise(resolve => setTimeout(resolve, 1000));
        }
      }
      
      expect(success).toBe(true);
    });

    test('should handle Redis cache failures gracefully', async () => {
      await api.disconnectRedis();
      
      const result = await api.executeCode('basic', 'PRINT "no cache"');
      expect(result.success).toBe(true);
      
      // Cache should reconnect
      const cached = await api.executeCode('basic', 'PRINT "with cache"');
      expect(cached.success).toBe(true);
    });

    test('should rollback failed transactions', async () => {
      const session = await api.createSession('test-rollback');
      
      // Try to update with invalid data
      try {
        await api.updateCode(session.id, null);
      } catch (e) {
        // Expected
      }
      
      // Session should still exist
      const retrieved = await api.getSession(session.id);
      expect(retrieved.id).toBe(session.id);
    });
  });

  // ===== MOBILE TESTS =====
  
  describe('Mobile Compatibility', () => {
    test('should work on iOS', async () => {
      const result = await api.executeCode(
        'basic',
        'PRINT "iOS"',
        { userAgent: 'iPad' }
      );
      
      expect(result.success).toBe(true);
    });

    test('should work on Android', async () => {
      const result = await api.executeCode(
        'basic',
        'PRINT "Android"',
        { userAgent: 'Android' }
      );
      
      expect(result.success).toBe(true);
    });

    test('should handle touch gestures', async () => {
      const session = await api.createSession('test-touch');
      
      const event = await api.sendTouchEvent(session.id, {
        type: 'tap',
        x: 100,
        y: 100
      });
      
      expect(event.handled).toBe(true);
    });

    test('should support offline mode', async () => {
      // Go offline
      await api.setNetworkMode('offline');
      
      // Execute code (should use cached module)
      const result = await api.executeCode('basic', 'PRINT "offline"');
      expect(result.success).toBe(true);
      
      // Go back online
      await api.setNetworkMode('online');
    });
  });
});
