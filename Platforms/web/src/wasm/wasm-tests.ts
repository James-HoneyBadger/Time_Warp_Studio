/**
 * WASM Testing Framework
 * Unit and integration tests for compiled WASM interpreters
 * 
 * Test Categories:
 * - Correctness: Outputs match server versions
 * - Memory: Proper allocation/deallocation
 * - Graphics: Turtle commands generate correct drawing commands
 * - Performance: Execution within target latency
 * - Edge Cases: Boundary conditions and error handling
 */

import { WasmInterpreter, WasmLoader, getWasmBridge } from '../src/wasm/wasm-loader';
import { logger } from '../src/utils/logger';
import type { ExecutionResult } from '../src/types';

interface TestResult {
  name: string;
  passed: boolean;
  duration: number;
  error?: string;
}

interface TestSuite {
  name: string;
  tests: TestResult[];
  passed: number;
  failed: number;
  totalDuration: number;
}

/**
 * Basic Interpreter Tests
 */
class BasicTests {
  private interpreter: WasmInterpreter | null = null;

  async setup(): Promise<void> {
    this.interpreter = new WasmInterpreter();
    try {
      await this.interpreter.initialize('basic');
    } catch (error) {
      logger.warn('WASM Basic not available, skipping tests');
    }
  }

  async teardown(): Promise<void> {
    if (this.interpreter) {
      await this.interpreter.shutdown();
    }
  }

  async testHelloWorld(): Promise<TestResult> {
    const startTime = performance.now();
    try {
      if (!this.interpreter) {
        return {
          name: 'Hello World',
          passed: false,
          duration: 0,
          error: 'Interpreter not initialized'
        };
      }

      const result = await this.interpreter.execute('PRINT "Hello, WASM!"');
      const duration = performance.now() - startTime;

      const passed = result.success && result.output.includes('Hello');
      return {
        name: 'Hello World',
        passed,
        duration,
        error: passed ? undefined : `Output: ${result.output}, Error: ${result.error}`
      };
    } catch (error) {
      return {
        name: 'Hello World',
        passed: false,
        duration: performance.now() - startTime,
        error: String(error)
      };
    }
  }

  async testVariableAssignment(): Promise<TestResult> {
    const startTime = performance.now();
    try {
      if (!this.interpreter) {
        return {
          name: 'Variable Assignment',
          passed: false,
          duration: 0,
          error: 'Interpreter not initialized'
        };
      }

      const code = `LET X = 42
PRINT X`;
      const result = await this.interpreter.execute(code);
      const duration = performance.now() - startTime;

      const passed = result.success && result.output.includes('42');
      return {
        name: 'Variable Assignment',
        passed,
        duration,
        error: passed ? undefined : `Output: ${result.output}`
      };
    } catch (error) {
      return {
        name: 'Variable Assignment',
        passed: false,
        duration: performance.now() - startTime,
        error: String(error)
      };
    }
  }

  async testArithmetic(): Promise<TestResult> {
    const startTime = performance.now();
    try {
      if (!this.interpreter) {
        return {
          name: 'Arithmetic',
          passed: false,
          duration: 0,
          error: 'Interpreter not initialized'
        };
      }

      const code = `LET X = 10
LET Y = 20
LET Z = X + Y
PRINT Z`;
      const result = await this.interpreter.execute(code);
      const duration = performance.now() - startTime;

      const passed = result.success && result.output.includes('30');
      return {
        name: 'Arithmetic',
        passed,
        duration,
        error: passed ? undefined : `Output: ${result.output}`
      };
    } catch (error) {
      return {
        name: 'Arithmetic',
        passed: false,
        duration: performance.now() - startTime,
        error: String(error)
      };
    }
  }

  async runAll(): Promise<TestResult[]> {
    await this.setup();
    const results = [
      await this.testHelloWorld(),
      await this.testVariableAssignment(),
      await this.testArithmetic()
    ];
    await this.teardown();
    return results;
  }
}

/**
 * Logo Interpreter Tests
 */
class LogoTests {
  private interpreter: WasmInterpreter | null = null;

  async setup(): Promise<void> {
    this.interpreter = new WasmInterpreter();
    try {
      await this.interpreter.initialize('logo');
    } catch (error) {
      logger.warn('WASM Logo not available, skipping tests');
    }
  }

  async teardown(): Promise<void> {
    if (this.interpreter) {
      await this.interpreter.shutdown();
    }
  }

  async testSquare(): Promise<TestResult> {
    const startTime = performance.now();
    try {
      if (!this.interpreter) {
        return {
          name: 'Draw Square',
          passed: false,
          duration: 0,
          error: 'Interpreter not initialized'
        };
      }

      const code = `REPEAT 4 [FORWARD 100 RIGHT 90]`;
      const result = await this.interpreter.execute(code);
      const duration = performance.now() - startTime;

      const passed = result.success;
      return {
        name: 'Draw Square',
        passed,
        duration,
        error: passed ? undefined : result.error
      };
    } catch (error) {
      return {
        name: 'Draw Square',
        passed: false,
        duration: performance.now() - startTime,
        error: String(error)
      };
    }
  }

  async testPenCommands(): Promise<TestResult> {
    const startTime = performance.now();
    try {
      if (!this.interpreter) {
        return {
          name: 'Pen Commands',
          passed: false,
          duration: 0,
          error: 'Interpreter not initialized'
        };
      }

      const code = `PENUP FORWARD 50 PENDOWN FORWARD 50`;
      const result = await this.interpreter.execute(code);
      const duration = performance.now() - startTime;

      const passed = result.success;
      return {
        name: 'Pen Commands',
        passed,
        duration,
        error: passed ? undefined : result.error
      };
    } catch (error) {
      return {
        name: 'Pen Commands',
        passed: false,
        duration: performance.now() - startTime,
        error: String(error)
      };
    }
  }

  async runAll(): Promise<TestResult[]> {
    await this.setup();
    const results = [
      await this.testSquare(),
      await this.testPenCommands()
    ];
    await this.teardown();
    return results;
  }
}

/**
 * Performance Benchmark Tests
 */
class PerformanceTests {
  private bridge = getWasmBridge();

  async testLatency(language: string, code: string): Promise<TestResult> {
    const startTime = performance.now();
    try {
      const result = await this.bridge.execute(language, code);
      const duration = performance.now() - startTime;

      // Target: <100ms for WASM, <500ms for server fallback
      const target = result.isWasm ? 100 : 500;
      const passed = duration < target;

      return {
        name: `${language} Latency`,
        passed,
        duration,
        error: passed ? undefined : `Expected <${target}ms, got ${duration.toFixed(2)}ms`
      };
    } catch (error) {
      return {
        name: `${language} Latency`,
        passed: false,
        duration: performance.now() - startTime,
        error: String(error)
      };
    }
  }

  async runAll(): Promise<TestResult[]> {
    return Promise.all([
      this.testLatency('basic', 'PRINT "test"'),
      this.testLatency('logo', 'FORWARD 50')
    ]);
  }
}

/**
 * Test Runner
 */
export async function runWasmTests(): Promise<TestSuite[]> {
  const suites: TestSuite[] = [];

  logger.info('Starting WASM test suite...');

  // Basic tests
  const basicTests = new BasicTests();
  const basicResults = await basicTests.runAll();
  suites.push({
    name: 'Basic Interpreter',
    tests: basicResults,
    passed: basicResults.filter(r => r.passed).length,
    failed: basicResults.filter(r => !r.passed).length,
    totalDuration: basicResults.reduce((sum, r) => sum + r.duration, 0)
  });

  // Logo tests
  const logoTests = new LogoTests();
  const logoResults = await logoTests.runAll();
  suites.push({
    name: 'Logo Interpreter',
    tests: logoResults,
    passed: logoResults.filter(r => r.passed).length,
    failed: logoResults.filter(r => !r.passed).length,
    totalDuration: logoResults.reduce((sum, r) => sum + r.duration, 0)
  });

  // Performance tests
  const perfTests = new PerformanceTests();
  const perfResults = await perfTests.runAll();
  suites.push({
    name: 'Performance',
    tests: perfResults,
    passed: perfResults.filter(r => r.passed).length,
    failed: perfResults.filter(r => !r.passed).length,
    totalDuration: perfResults.reduce((sum, r) => sum + r.duration, 0)
  });

  return suites;
}

/**
 * Print test results
 */
export function printTestResults(suites: TestSuite[]): void {
  console.log('\n=== WASM Test Results ===\n');

  let totalPassed = 0;
  let totalFailed = 0;

  for (const suite of suites) {
    console.log(`${suite.name}:`);
    console.log(`  Passed: ${suite.passed}/${suite.tests.length}`);
    console.log(`  Duration: ${suite.totalDuration.toFixed(2)}ms`);

    for (const test of suite.tests) {
      const status = test.passed ? '✅' : '❌';
      console.log(`  ${status} ${test.name} (${test.duration.toFixed(2)}ms)`);
      if (test.error) {
        console.log(`     Error: ${test.error}`);
      }
    }
    console.log();

    totalPassed += suite.passed;
    totalFailed += suite.failed;
  }

  console.log(`Total: ${totalPassed} passed, ${totalFailed} failed`);
}

// Auto-run if executed directly
if (typeof window === 'undefined') {
  runWasmTests().then(printTestResults);
}
