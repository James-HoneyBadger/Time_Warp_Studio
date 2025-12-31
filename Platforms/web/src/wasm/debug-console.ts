/**
 * WASM Error Reporting and Debugging Infrastructure
 * Provides error propagation, stack traces, and debugging support
 * Phase 5.3: WASM Runtime & Integration
 */

export enum ErrorSeverity {
  INFO = 'info',
  WARNING = 'warning',
  ERROR = 'error',
  CRITICAL = 'critical',
}

export interface WasmError {
  code: number;
  message: string;
  context: string;
  line?: number;
  column?: number;
  stackTrace?: string;
  severity: ErrorSeverity;
  timestamp: number;
}

export interface DebugInfo {
  breakpoints: Set<number>;
  watchVars: Set<string>;
  executionTraces: ExecutionTrace[];
  isPaused: boolean;
}

export interface ExecutionTrace {
  functionName: string;
  line: number;
  timestamp: number;
  locals: Record<string, any>;
  stackDepth: number;
}

/**
 * ErrorReporter: Centralized error reporting for WASM execution
 */
export class ErrorReporter {
  private errors: WasmError[] = [];
  private maxErrors: number = 100;
  private errorListeners: Array<(error: WasmError) => void> = [];
  private errorMap: Record<number, string> = {
    0: 'Success',
    1: 'Syntax Error',
    2: 'Runtime Error',
    3: 'Memory Error',
    4: 'Stack Overflow',
    5: 'Type Error',
    6: 'Undefined Variable',
    7: 'Index Out of Bounds',
    8: 'Execution Timeout',
    255: 'Unknown Error',
  };

  /**
   * Report error
   */
  reportError(
    code: number,
    message: string,
    context: string = '',
    line?: number,
    column?: number
  ): WasmError {
    const error: WasmError = {
      code,
      message,
      context,
      line,
      column,
      severity: this.getSeverity(code),
      timestamp: Date.now(),
      stackTrace: new Error().stack,
    };

    this.errors.push(error);

    // Trim old errors
    if (this.errors.length > this.maxErrors) {
      this.errors = this.errors.slice(-this.maxErrors);
    }

    // Notify listeners
    for (const listener of this.errorListeners) {
      listener(error);
    }

    this.logError(error);

    return error;
  }

  /**
   * Get error message by code
   */
  getErrorMessage(code: number): string {
    return this.errorMap[code] || 'Unknown error';
  }

  /**
   * Determine severity from error code
   */
  private getSeverity(code: number): ErrorSeverity {
    if (code === 0) return ErrorSeverity.INFO;
    if (code <= 2) return ErrorSeverity.ERROR;
    if (code <= 5) return ErrorSeverity.WARNING;
    return ErrorSeverity.CRITICAL;
  }

  /**
   * Log error
   */
  private logError(error: WasmError): void {
    const message = `[${error.severity.toUpperCase()}] ${error.message}`;
    const details = error.line ? ` at line ${error.line}` : '';

    switch (error.severity) {
      case ErrorSeverity.INFO:
        console.info(message + details);
        break;
      case ErrorSeverity.WARNING:
        console.warn(message + details);
        break;
      case ErrorSeverity.ERROR:
        console.error(message + details);
        break;
      case ErrorSeverity.CRITICAL:
        console.error(`🚨 CRITICAL: ${message}${details}`);
        break;
    }

    if (error.context) {
      console.debug(`Context: ${error.context}`);
    }
  }

  /**
   * Subscribe to errors
   */
  onError(listener: (error: WasmError) => void): () => void {
    this.errorListeners.push(listener);
    return () => {
      const index = this.errorListeners.indexOf(listener);
      if (index > -1) {
        this.errorListeners.splice(index, 1);
      }
    };
  }

  /**
   * Get all errors
   */
  getErrors(): WasmError[] {
    return [...this.errors];
  }

  /**
   * Get recent errors
   */
  getRecentErrors(count: number = 10): WasmError[] {
    return this.errors.slice(-count);
  }

  /**
   * Clear errors
   */
  clear(): void {
    this.errors = [];
  }

  /**
   * Get errors by severity
   */
  getErrorsBySeverity(severity: ErrorSeverity): WasmError[] {
    return this.errors.filter((e) => e.severity === severity);
  }

  /**
   * Format error for display
   */
  formatError(error: WasmError): string {
    let formatted = `${error.message}`;

    if (error.line !== undefined) {
      formatted += ` (line ${error.line}`;
      if (error.column !== undefined) {
        formatted += `, col ${error.column}`;
      }
      formatted += ')';
    }

    if (error.context) {
      formatted += `\nContext: ${error.context}`;
    }

    return formatted;
  }
}

/**
 * Debugger: WASM code debugger
 */
export class WasmDebugger {
  private breakpoints: Map<number, { condition?: string; hitCount: number }> = new Map();
  private watchVariables: Set<string> = new Set();
  private executionTraces: ExecutionTrace[] = [];
  private isPaused: boolean = false;
  private stepMode: 'run' | 'step' | 'stepover' | 'stepout' = 'run';
  private maxTraces: number = 1000;

  /**
   * Set breakpoint
   */
  setBreakpoint(line: number, condition?: string): void {
    this.breakpoints.set(line, { condition, hitCount: 0 });
  }

  /**
   * Remove breakpoint
   */
  removeBreakpoint(line: number): void {
    this.breakpoints.delete(line);
  }

  /**
   * Check if breakpoint hit
   */
  checkBreakpoint(line: number, context?: Record<string, any>): boolean {
    const bp = this.breakpoints.get(line);
    if (!bp) return false;

    bp.hitCount++;

    if (bp.condition && context) {
      try {
        return this.evaluateCondition(bp.condition, context);
      } catch {
        return true; // Hit on error
      }
    }

    return true;
  }

  /**
   * Evaluate breakpoint condition
   */
  private evaluateCondition(condition: string, context: Record<string, any>): boolean {
    // Simple condition evaluation
    const vars = Object.entries(context)
      .map(([k, v]) => `let ${k} = ${JSON.stringify(v)};`)
      .join('');

    try {
      const result = new Function(`${vars} return ${condition}`)();
      return Boolean(result);
    } catch {
      return false;
    }
  }

  /**
   * Add watch variable
   */
  addWatch(variable: string): void {
    this.watchVariables.add(variable);
  }

  /**
   * Remove watch variable
   */
  removeWatch(variable: string): void {
    this.watchVariables.delete(variable);
  }

  /**
   * Get watched variables
   */
  getWatchVariables(): string[] {
    return Array.from(this.watchVariables);
  }

  /**
   * Record execution trace
   */
  recordTrace(
    functionName: string,
    line: number,
    locals: Record<string, any>,
    stackDepth: number
  ): void {
    const trace: ExecutionTrace = {
      functionName,
      line,
      timestamp: performance.now(),
      locals: { ...locals },
      stackDepth,
    };

    this.executionTraces.push(trace);

    // Trim old traces
    if (this.executionTraces.length > this.maxTraces) {
      this.executionTraces = this.executionTraces.slice(-this.maxTraces);
    }
  }

  /**
   * Get execution traces
   */
  getTraces(count?: number): ExecutionTrace[] {
    if (count) {
      return this.executionTraces.slice(-count);
    }
    return [...this.executionTraces];
  }

  /**
   * Clear traces
   */
  clearTraces(): void {
    this.executionTraces = [];
  }

  /**
   * Pause execution
   */
  pause(): void {
    this.isPaused = true;
    this.stepMode = 'run';
  }

  /**
   * Resume execution
   */
  resume(): void {
    this.isPaused = false;
    this.stepMode = 'run';
  }

  /**
   * Step into
   */
  stepInto(): void {
    this.stepMode = 'step';
    this.isPaused = false;
  }

  /**
   * Step over
   */
  stepOver(): void {
    this.stepMode = 'stepover';
    this.isPaused = false;
  }

  /**
   * Step out
   */
  stepOut(): void {
    this.stepMode = 'stepout';
    this.isPaused = false;
  }

  /**
   * Get debugger state
   */
  getState(): DebugInfo {
    return {
      breakpoints: new Set(this.breakpoints.keys()),
      watchVars: new Set(this.watchVariables),
      executionTraces: [...this.executionTraces],
      isPaused: this.isPaused,
    };
  }

  /**
   * Format trace for display
   */
  formatTrace(trace: ExecutionTrace): string {
    const time = trace.timestamp.toFixed(2);
    const vars = Object.entries(trace.locals)
      .map(([k, v]) => `${k}=${JSON.stringify(v)}`)
      .join(', ');

    return `[${time}ms] ${trace.functionName}:${trace.line} (stack=${trace.stackDepth}) {${vars}}`;
  }
}

/**
 * PerformanceProfiler: Profile WASM execution performance
 */
export class PerformanceProfiler {
  private profiles: Map<string, { calls: number; totalTime: number; minTime: number; maxTime: number }> =
    new Map();
  private currentProfile: string | null = null;
  private startTime: number = 0;

  /**
   * Start profiling function
   */
  startFunction(name: string): void {
    if (this.currentProfile) {
      this.endFunction();
    }
    this.currentProfile = name;
    this.startTime = performance.now();
  }

  /**
   * End profiling function
   */
  endFunction(): void {
    if (!this.currentProfile) return;

    const duration = performance.now() - this.startTime;
    const profile = this.profiles.get(this.currentProfile) || {
      calls: 0,
      totalTime: 0,
      minTime: duration,
      maxTime: duration,
    };

    profile.calls++;
    profile.totalTime += duration;
    profile.minTime = Math.min(profile.minTime, duration);
    profile.maxTime = Math.max(profile.maxTime, duration);

    this.profiles.set(this.currentProfile, profile);
    this.currentProfile = null;
  }

  /**
   * Get profile for function
   */
  getProfile(name: string) {
    return this.profiles.get(name);
  }

  /**
   * Get all profiles
   */
  getProfiles() {
    const result: Record<string, { calls: number; totalTime: number; avgTime: number; minTime: number; maxTime: number }> = {};

    for (const [name, profile] of this.profiles.entries()) {
      result[name] = {
        calls: profile.calls,
        totalTime: profile.totalTime,
        avgTime: profile.totalTime / profile.calls,
        minTime: profile.minTime,
        maxTime: profile.maxTime,
      };
    }

    return result;
  }

  /**
   * Clear profiles
   */
  clear(): void {
    this.profiles.clear();
    this.currentProfile = null;
  }

  /**
   * Get slowest functions
   */
  getSlowest(count: number = 10): Array<{ name: string; avgTime: number }> {
    const all = this.getProfiles();
    return Object.entries(all)
      .map(([name, profile]) => ({
        name,
        avgTime: profile.avgTime,
      }))
      .sort((a, b) => b.avgTime - a.avgTime)
      .slice(0, count);
  }

  /**
   * Format profile data
   */
  formatProfiles(): string {
    const profiles = this.getProfiles();
    let output = 'Function Performance Profile\n';
    output += '============================\n';

    for (const [name, profile] of Object.entries(profiles)) {
      output += `${name}:\n`;
      output += `  Calls: ${profile.calls}\n`;
      output += `  Total: ${profile.totalTime.toFixed(2)}ms\n`;
      output += `  Avg:   ${profile.avgTime.toFixed(2)}ms\n`;
      output += `  Min:   ${profile.minTime.toFixed(2)}ms\n`;
      output += `  Max:   ${profile.maxTime.toFixed(2)}ms\n`;
      output += '\n';
    }

    return output;
  }
}

/**
 * DebugConsole: Unified debugging console
 */
export class DebugConsole {
  private errorReporter: ErrorReporter;
  private debugger: WasmDebugger;
  private profiler: PerformanceProfiler;
  private logs: Array<{ level: 'log' | 'warn' | 'error' | 'info'; message: string; timestamp: number }> =
    [];

  constructor(
    errorReporter: ErrorReporter,
    debugger: WasmDebugger,
    profiler: PerformanceProfiler
  ) {
    this.errorReporter = errorReporter;
    this.debugger = debugger;
    this.profiler = profiler;
  }

  /**
   * Log message
   */
  log(message: string, level: 'log' | 'warn' | 'error' | 'info' = 'log'): void {
    this.logs.push({
      level,
      message,
      timestamp: Date.now(),
    });

    console[level](message);
  }

  /**
   * Get all logs
   */
  getLogs() {
    return [...this.logs];
  }

  /**
   * Get debug report
   */
  getDebugReport(): string {
    let report = '=== WASM DEBUG REPORT ===\n\n';

    // Errors
    report += '--- ERRORS ---\n';
    const errors = this.errorReporter.getErrors();
    if (errors.length > 0) {
      for (const error of errors.slice(-5)) {
        report += `${error.severity}: ${error.message}\n`;
      }
    } else {
      report += 'No errors\n';
    }

    // Debugger state
    report += '\n--- DEBUGGER STATE ---\n';
    const debugState = this.debugger.getState();
    report += `Paused: ${debugState.isPaused}\n`;
    report += `Breakpoints: ${debugState.breakpoints.size}\n`;
    report += `Watched vars: ${debugState.watchVars.size}\n`;
    report += `Traces: ${debugState.executionTraces.length}\n`;

    // Performance
    report += '\n--- PERFORMANCE ---\n';
    const slowest = this.profiler.getSlowest(5);
    for (const func of slowest) {
      report += `${func.name}: ${func.avgTime.toFixed(2)}ms\n`;
    }

    return report;
  }

  /**
   * Clear all debug data
   */
  clear(): void {
    this.errorReporter.clear();
    this.debugger.clearTraces();
    this.profiler.clear();
    this.logs = [];
  }
}
