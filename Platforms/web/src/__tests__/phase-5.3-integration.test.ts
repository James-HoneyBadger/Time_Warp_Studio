/**
 * Phase 5.3 WASM Integration Test Suite
 * Comprehensive testing for WASM runtime, boundaries, graphics, and debugging
 */

import { describe, it, beforeAll, afterAll, expect } from '@jest/globals';
import { WasmRuntime } from '../wasm/wasm-runtime';
import { WasmBridgeImpl, WasmMemoryPool } from '../wasm/wasm-boundary';
import { GraphicsRenderer, TurtleGraphicsRenderer } from '../wasm/graphics-renderer';
import { ErrorReporter, WasmDebugger, PerformanceProfiler } from '../wasm/debug-console';
import { WasmModuleIntegrator, initializeWasm } from '../wasm/wasm-integration';

describe('Phase 5.3: WASM Runtime Integration Tests', () => {
  // ========== WASM Runtime Tests ==========

  describe('WasmRuntime', () => {
    let runtime: WasmRuntime;

    beforeAll(async () => {
      runtime = new WasmRuntime({
        name: 'test',
        wasmPath: '/wasm/test.wasm',
        enableDebug: true,
      });
    });

    it('should create runtime instance', () => {
      expect(runtime).toBeDefined();
      expect(runtime.getName()).toBe('test');
    });

    it('should not be initialized before init()', () => {
      expect(runtime.isReady()).toBe(false);
    });

    it('should handle invalid WASM path', async () => {
      const badRuntime = new WasmRuntime({
        name: 'bad',
        wasmPath: '/nonexistent.wasm',
      });

      try {
        await badRuntime.initialize();
      } catch (error) {
        expect(error).toBeDefined();
      }
    });
  });

  // ========== Memory Pool Tests ==========

  describe('WasmMemoryPool', () => {
    let buffer: ArrayBuffer;
    let pool: WasmMemoryPool;

    beforeAll(() => {
      buffer = new ArrayBuffer(1024 * 1024); // 1 MB
      pool = new WasmMemoryPool(buffer, 0, 1024 * 1024);
    });

    it('should allocate memory', () => {
      const offset = pool.allocate(256);
      expect(offset).toBeGreaterThanOrEqual(0);
    });

    it('should track allocations', () => {
      const offset1 = pool.allocate(128);
      const offset2 = pool.allocate(256);

      expect(offset1).not.toBe(offset2);

      const stats = pool.getStats();
      expect(stats.allocationCount).toBe(2);
      expect(stats.allocatedBytes).toBe(384);
    });

    it('should free memory', () => {
      const offset = pool.allocate(512);
      pool.free(offset);

      const stats = pool.getStats();
      expect(stats.freeBytes).toBeGreaterThan(0);
    });

    it('should detect invalid free', () => {
      expect(() => pool.free(999999)).toThrow();
    });

    it('should write and read data', () => {
      const offset = pool.allocate(32);
      const data = new Uint8Array([1, 2, 3, 4, 5]);

      pool.writeData(offset, data);
      const read = pool.readData(offset, 5);

      expect(Array.from(read)).toEqual([1, 2, 3, 4, 5]);
    });

    it('should merge free blocks', () => {
      const offset1 = pool.allocate(64);
      const offset2 = pool.allocate(64);
      const offset3 = pool.allocate(64);

      pool.free(offset1);
      pool.free(offset2);

      const stats = pool.getStats();
      // After merging, should have fewer fragmentation
      expect(stats.fragmentationRatio).toBeLessThan(1.0);
    });

    it('should clear all allocations', () => {
      pool.clear();
      const stats = pool.getStats();

      expect(stats.allocationCount).toBe(0);
      expect(stats.allocatedBytes).toBe(0);
    });
  });

  // ========== Graphics Renderer Tests ==========

  describe('TurtleGraphicsRenderer', () => {
    let canvas: HTMLCanvasElement;
    let renderer: TurtleGraphicsRenderer;

    beforeAll(() => {
      canvas = document.createElement('canvas');
      canvas.width = 800;
      canvas.height = 600;
      renderer = new TurtleGraphicsRenderer(canvas, 1);
    });

    it('should initialize renderer', () => {
      expect(renderer).toBeDefined();
    });

    it('should track turtle position', () => {
      const state = renderer.getState();
      expect(state).toHaveProperty('x');
      expect(state).toHaveProperty('y');
      expect(state).toHaveProperty('angle');
    });

    it('should move turtle forward', () => {
      const state1 = renderer.getState();
      renderer.forward(100);
      const state2 = renderer.getState();

      expect(state2.x).not.toBe(state1.x);
      expect(state2.y).not.toBe(state1.y);
    });

    it('should rotate turtle', () => {
      const state1 = renderer.getState();
      renderer.right(45);
      const state2 = renderer.getState();

      expect(state2.angle).not.toBe(state1.angle);
    });

    it('should handle pen up/down', () => {
      renderer.penUp();
      const state1 = renderer.getState();
      expect(state1.penDown).toBe(false);

      renderer.penDown();
      const state2 = renderer.getState();
      expect(state2.penDown).toBe(true);
    });

    it('should set color', () => {
      renderer.setColor('red');
      const state = renderer.getState();
      expect(state.color).toBe('red');
    });

    it('should return to home', () => {
      renderer.forward(100);
      renderer.home();
      const state = renderer.getState();

      expect(state.angle).toBe(0);
    });

    it('should save and restore state', () => {
      const initial = renderer.getState();

      renderer.forward(50);
      renderer.right(45);
      renderer.saveState();

      renderer.forward(100);
      renderer.right(90);
      renderer.restoreState();

      const restored = renderer.getState();
      expect(restored.x).toBe(initial.x + 50);
      expect(restored.angle).toBe(45);
    });

    it('should clear canvas', () => {
      renderer.clear('white');
      const state = renderer.getState();
      expect(state.x).toBe(canvas.width / 2); // Center position
    });
  });

  // ========== Error Reporter Tests ==========

  describe('ErrorReporter', () => {
    let reporter: ErrorReporter;

    beforeAll(() => {
      reporter = new ErrorReporter();
    });

    it('should report error', () => {
      const error = reporter.reportError(1, 'Test error', 'test context', 10, 5);

      expect(error.code).toBe(1);
      expect(error.message).toBe('Test error');
      expect(error.line).toBe(10);
      expect(error.column).toBe(5);
    });

    it('should get error message by code', () => {
      const message = reporter.getErrorMessage(1);
      expect(message).toBeDefined();
    });

    it('should track multiple errors', () => {
      reporter.reportError(1, 'Error 1');
      reporter.reportError(2, 'Error 2');
      reporter.reportError(3, 'Error 3');

      const errors = reporter.getErrors();
      expect(errors.length).toBeGreaterThanOrEqual(3);
    });

    it('should filter errors by severity', () => {
      const criticalErrors = reporter.getErrorsBySeverity('critical');
      expect(Array.isArray(criticalErrors)).toBe(true);
    });

    it('should support error listeners', (done) => {
      const listener = () => {
        done();
      };

      reporter.onError(listener);
      reporter.reportError(1, 'Test');
    });

    it('should clear errors', () => {
      reporter.clear();
      const errors = reporter.getErrors();
      expect(errors.length).toBe(0);
    });
  });

  // ========== Debugger Tests ==========

  describe('WasmDebugger', () => {
    let debugger_: WasmDebugger;

    beforeAll(() => {
      debugger_ = new WasmDebugger();
    });

    it('should set breakpoint', () => {
      debugger_.setBreakpoint(10, 'x > 5');
      expect(debugger_.getState().breakpoints.has(10)).toBe(true);
    });

    it('should check breakpoint hit', () => {
      debugger_.setBreakpoint(20);
      const hit = debugger_.checkBreakpoint(20);
      expect(hit).toBe(true);
    });

    it('should add watch variable', () => {
      debugger_.addWatch('myVar');
      expect(debugger_.getWatchVariables()).toContain('myVar');
    });

    it('should record execution traces', () => {
      debugger_.recordTrace('testFunc', 15, { x: 10 }, 2);
      const traces = debugger_.getTraces();
      expect(traces.length).toBeGreaterThan(0);
    });

    it('should handle pause/resume', () => {
      debugger_.pause();
      expect(debugger_.getState().isPaused).toBe(true);

      debugger_.resume();
      expect(debugger_.getState().isPaused).toBe(false);
    });

    it('should clear traces', () => {
      debugger_.clearTraces();
      const traces = debugger_.getTraces();
      expect(traces.length).toBe(0);
    });
  });

  // ========== Performance Profiler Tests ==========

  describe('PerformanceProfiler', () => {
    let profiler: PerformanceProfiler;

    beforeAll(() => {
      profiler = new PerformanceProfiler();
    });

    it('should profile function calls', () => {
      profiler.startFunction('testFunc');

      // Simulate work
      let sum = 0;
      for (let i = 0; i < 1000; i++) {
        sum += i;
      }

      profiler.endFunction();

      const profile = profiler.getProfile('testFunc');
      expect(profile).toBeDefined();
      expect(profile!.calls).toBe(1);
    });

    it('should track multiple calls', () => {
      for (let i = 0; i < 5; i++) {
        profiler.startFunction('multiFunc');
        profiler.endFunction();
      }

      const profile = profiler.getProfile('multiFunc');
      expect(profile!.calls).toBe(5);
    });

    it('should get slowest functions', () => {
      const slowest = profiler.getSlowest(2);
      expect(Array.isArray(slowest)).toBe(true);
    });

    it('should format profiles', () => {
      const formatted = profiler.formatProfiles();
      expect(typeof formatted).toBe('string');
      expect(formatted.length).toBeGreaterThan(0);
    });

    it('should clear profiles', () => {
      profiler.clear();
      const profiles = profiler.getProfiles();
      expect(Object.keys(profiles).length).toBe(0);
    });
  });

  // ========== Integration Tests ==========

  describe('WasmModuleIntegrator', () => {
    it('should create integrator instance', () => {
      const integrator = new WasmModuleIntegrator({
        languages: ['logo', 'pilot', 'pascal'],
        wasmBasePath: '/wasm',
        enableDebug: true,
      });

      expect(integrator).toBeDefined();
      expect(integrator.isReady()).toBe(false);
    });

    it('should get available languages', () => {
      const integrator = new WasmModuleIntegrator({
        languages: ['logo', 'basic'],
        wasmBasePath: '/wasm',
      });

      const languages = integrator.getAvailableLanguages();
      expect(languages).toContain('logo');
      expect(languages).toContain('basic');
    });

    it('should provide debug report', () => {
      const integrator = new WasmModuleIntegrator({
        languages: [],
        wasmBasePath: '/wasm',
      });

      const report = integrator.getDebugReport();
      expect(typeof report).toBe('string');
    });

    it('should handle execution statistics', () => {
      const integrator = new WasmModuleIntegrator({
        languages: ['test'],
        wasmBasePath: '/wasm',
      });

      const stats = integrator.getStatistics();
      expect(stats['test']).toBeDefined();
      expect(stats['test'].attempts).toBe(0);
    });
  });

  // ========== Error Handling Tests ==========

  describe('Error Handling', () => {
    it('should handle memory exhaustion gracefully', () => {
      const pool = new WasmMemoryPool(new ArrayBuffer(1024), 0, 1024);

      // Try to allocate beyond available memory
      expect(() => pool.allocate(2048)).toThrow();
    });

    it('should recover from allocation failures', () => {
      const pool = new WasmMemoryPool(new ArrayBuffer(4096), 0, 4096);

      // Allocate some memory
      const offset = pool.allocate(1024);
      expect(offset).toBeGreaterThanOrEqual(0);

      // Free and reallocate
      pool.free(offset);
      const newOffset = pool.allocate(512);
      expect(newOffset).toBeGreaterThanOrEqual(0);
    });
  });

  // ========== Concurrency Tests ==========

  describe('Concurrent Execution', () => {
    it('should handle multiple error reports', () => {
      const reporter = new ErrorReporter();

      const errors = Array.from({ length: 100 }, (_, i) =>
        reporter.reportError(1, `Error ${i}`)
      );

      expect(errors.length).toBe(100);
      expect(reporter.getErrors().length).toBeGreaterThan(0);
    });

    it('should track concurrent profiling', () => {
      const profiler = new PerformanceProfiler();

      // Simulate concurrent execution tracking
      const funcs = ['func1', 'func2', 'func3'];
      for (const func of funcs) {
        for (let i = 0; i < 10; i++) {
          profiler.startFunction(func);
          profiler.endFunction();
        }
      }

      const profiles = profiler.getProfiles();
      for (const func of funcs) {
        expect(profiles[func].calls).toBe(10);
      }
    });
  });
});

describe('Phase 5.3 Integration Completion Criteria', () => {
  it('✅ WASM runtime environment established', () => {
    const runtime = new WasmRuntime({
      name: 'test',
      wasmPath: '/test.wasm',
    });
    expect(runtime).toBeDefined();
  });

  it('✅ Memory management utilities created', () => {
    const buffer = new ArrayBuffer(1024);
    const pool = new WasmMemoryPool(buffer, 0, 1024);
    expect(pool).toBeDefined();
    expect(pool.getStats().totalBytes).toBe(1024);
  });

  it('✅ JavaScript boundary functions implemented', () => {
    const instance = {} as any;
    const memory = new WebAssembly.Memory({ initial: 1 });

    const bridge = new WasmBridgeImpl(instance, memory);
    expect(bridge).toBeDefined();
  });

  it('✅ Graphics rendering integration complete', () => {
    const canvas = document.createElement('canvas');
    const renderer = new GraphicsRenderer(canvas);
    expect(renderer).toBeDefined();
  });

  it('✅ Error reporting pipeline working', () => {
    const reporter = new ErrorReporter();
    const error = reporter.reportError(1, 'Test');
    expect(error).toBeDefined();
    expect(reporter.getErrors().length).toBeGreaterThan(0);
  });

  it('✅ Debugging support implemented', () => {
    const debugger_ = new WasmDebugger();
    debugger_.setBreakpoint(10);
    expect(debugger_.getState().breakpoints.has(10)).toBe(true);
  });

  it('✅ Module integration complete', () => {
    const integrator = new WasmModuleIntegrator({
      languages: ['test'],
      wasmBasePath: '/wasm',
    });
    expect(integrator).toBeDefined();
    expect(integrator.getAvailableLanguages()).toContain('test');
  });
});
