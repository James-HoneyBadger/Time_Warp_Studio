/**
 * WASM Module Integration System
 * Unified module management for all WASM interpreters with fallback support
 * Phase 5.3: WASM Runtime & Integration
 */

import { WasmRuntime, WasmRuntimePool, ExecutionResult } from './wasm-runtime';
import { WasmBridgeImpl, BoundaryModule, WasmErrorBoundary } from './wasm-boundary';
import { GraphicsRenderer, CanvasManager } from './graphics-renderer';
import { ErrorReporter, WasmDebugger, PerformanceProfiler, DebugConsole } from './debug-console';

export interface ModuleConfig {
  languages: string[];
  wasmBasePath: string;
  enableDebug?: boolean;
  enableProfiling?: boolean;
  maxExecutionTime?: number;
  fallbackUrl?: string; // Server fallback URL
  canvases?: Map<string, HTMLCanvasElement>;
}

export interface ExecutionOptions {
  language: string;
  code: string;
  timeout?: number;
  debug?: boolean;
  profile?: boolean;
}

export interface ExecutionResponse {
  success: boolean;
  output: string;
  error?: string;
  graphics?: any[];
  stats: {
    duration: number;
    memoryUsed: number;
    executionMode: 'wasm' | 'fallback' | 'error';
  };
}

/**
 * WasmModuleIntegrator: Main integration point for all WASM functionality
 */
export class WasmModuleIntegrator {
  private runtimePool: WasmRuntimePool;
  private errorReporter: ErrorReporter;
  private debugger: WasmDebugger;
  private profiler: PerformanceProfiler;
  private debugConsole: DebugConsole;
  private errorBoundary: WasmErrorBoundary;
  private canvasManager: CanvasManager;
  private config: ModuleConfig;
  private isInitialized: boolean = false;
  private fallbackAvailable: boolean = false;
  private executionStats: Map<string, { attempts: number; successes: number; failures: number }> =
    new Map();

  constructor(config: ModuleConfig) {
    this.config = config;
    this.runtimePool = new WasmRuntimePool(config.enableDebug);
    this.errorReporter = new ErrorReporter();
    this.debugger = new WasmDebugger();
    this.profiler = new PerformanceProfiler();
    this.errorBoundary = new WasmErrorBoundary();
    this.debugConsole = new DebugConsole(this.errorReporter, this.debugger, this.profiler);
    this.canvasManager = new CanvasManager();

    // Register canvases
    if (config.canvases) {
      for (const [name, canvas] of config.canvases.entries()) {
        this.canvasManager.register(name, canvas);
      }
    }
  }

  /**
   * Initialize all WASM modules
   */
  async initialize(): Promise<void> {
    if (this.isInitialized) {
      return;
    }

    try {
      console.log('🚀 Initializing WASM Module Integration...');

      // Register runtimes for each language
      for (const language of this.config.languages) {
        const wasmPath = `${this.config.wasmBasePath}/${language}.wasm`;

        const runtime = new WasmRuntime({
          name: language,
          wasmPath,
          enableDebug: this.config.enableDebug,
          maxExecutionTime: this.config.maxExecutionTime,
        });

        this.runtimePool.register(language, runtime);

        // Initialize execution stats
        this.executionStats.set(language, {
          attempts: 0,
          successes: 0,
          failures: 0,
        });
      }

      // Initialize all runtimes
      await this.runtimePool.initializeAll();

      // Check fallback availability
      this.fallbackAvailable = await this.checkFallback();

      this.isInitialized = true;
      console.log('✅ WASM Module Integration ready');

      // Log available languages
      const languages = this.runtimePool.getAvailableLanguages();
      console.log(`📚 Available languages: ${languages.join(', ')}`);
    } catch (error) {
      this.errorReporter.reportError(
        3, // Memory error
        `Initialization failed: ${error}`,
        'WasmModuleIntegrator.initialize'
      );
      throw error;
    }
  }

  /**
   * Check if server fallback is available
   */
  private async checkFallback(): Promise<boolean> {
    if (!this.config.fallbackUrl) {
      return false;
    }

    try {
      const response = await fetch(`${this.config.fallbackUrl}/health`, {
        method: 'GET',
        timeout: 2000,
      } as any);

      return response.ok;
    } catch {
      return false;
    }
  }

  /**
   * Execute code with automatic fallback
   */
  async execute(options: ExecutionOptions): Promise<ExecutionResponse> {
    if (!this.isInitialized) {
      await this.initialize();
    }

    const stats = this.executionStats.get(options.language);
    if (stats) {
      stats.attempts++;
    }

    try {
      // Try WASM execution first
      const result = await this.executeWasm(options);

      if (result.success) {
        if (stats) stats.successes++;
        return {
          success: true,
          output: result.output,
          graphics: result.graphics,
          stats: {
            duration: result.duration,
            memoryUsed: result.memoryUsed,
            executionMode: 'wasm',
          },
        };
      }

      // WASM failed, try fallback
      console.warn(`WASM execution failed for ${options.language}, trying fallback...`);

      if (this.fallbackAvailable) {
        const fallbackResult = await this.executeFallback(options);
        if (fallbackResult) {
          if (stats) stats.successes++;
          return {
            success: true,
            output: fallbackResult.output,
            graphics: fallbackResult.graphics,
            stats: {
              duration: fallbackResult.duration,
              memoryUsed: 0,
              executionMode: 'fallback',
            },
          };
        }
      }

      // Both failed
      if (stats) stats.failures++;
      return {
        success: false,
        output: '',
        error: result.error || 'Execution failed',
        stats: {
          duration: result.duration,
          memoryUsed: result.memoryUsed,
          executionMode: 'error',
        },
      };
    } catch (error) {
      if (stats) stats.failures++;

      this.errorBoundary.reportError(
        error as Error,
        `Execution error for ${options.language}`
      );

      return {
        success: false,
        output: '',
        error: `Execution error: ${error}`,
        stats: {
          duration: 0,
          memoryUsed: 0,
          executionMode: 'error',
        },
      };
    }
  }

  /**
   * Execute code in WASM
   */
  private async executeWasm(options: ExecutionOptions): Promise<ExecutionResult> {
    const runtime = this.runtimePool.get(options.language);

    if (!runtime) {
      return {
        success: false,
        output: '',
        error: `Unknown language: ${options.language}`,
        duration: 0,
        memoryUsed: 0,
      };
    }

    if (options.profile && this.config.enableProfiling) {
      this.profiler.startFunction(`execute_${options.language}`);
    }

    try {
      const result = await runtime.execute(options.code);

      // Render graphics if available
      if (result.graphics && result.graphics.length > 0) {
        this.renderGraphics(options.language, result.graphics);
      }

      if (options.profile && this.config.enableProfiling) {
        this.profiler.endFunction();
      }

      return result;
    } catch (error) {
      if (options.profile && this.config.enableProfiling) {
        this.profiler.endFunction();
      }

      return {
        success: false,
        output: '',
        error: `WASM execution error: ${error}`,
        duration: 0,
        memoryUsed: 0,
      };
    }
  }

  /**
   * Execute code via server fallback
   */
  private async executeFallback(
    options: ExecutionOptions
  ): Promise<{ output: string; graphics?: any[]; duration: number } | null> {
    if (!this.config.fallbackUrl) {
      return null;
    }

    try {
      const startTime = performance.now();

      const response = await fetch(`${this.config.fallbackUrl}/execute`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          language: options.language,
          code: options.code,
          timeout: options.timeout || 30000,
        }),
      });

      if (!response.ok) {
        throw new Error(`Server error: ${response.statusText}`);
      }

      const data = await response.json();

      return {
        output: data.output || '',
        graphics: data.graphics,
        duration: performance.now() - startTime,
      };
    } catch (error) {
      console.error('Fallback execution failed:', error);
      return null;
    }
  }

  /**
   * Render graphics commands
   */
  private renderGraphics(language: string, commands: any[]): void {
    if (language === 'logo') {
      const renderer = this.canvasManager.getRenderer('logo');
      if (renderer) {
        renderer.render(commands);
      }
    }
  }

  /**
   * Get execution statistics
   */
  getStatistics() {
    const stats: Record<string, { attempts: number; successRate: number }> = {};

    for (const [language, data] of this.executionStats.entries()) {
      stats[language] = {
        attempts: data.attempts,
        successRate: data.attempts > 0 ? (data.successes / data.attempts) * 100 : 0,
      };
    }

    return stats;
  }

  /**
   * Get debug report
   */
  getDebugReport(): string {
    return this.debugConsole.getDebugReport();
  }

  /**
   * Set debug mode
   */
  setDebugMode(enabled: boolean): void {
    this.runtimePool.setDebugMode(enabled);
    if (enabled) {
      console.log('🐛 Debug mode enabled');
    }
  }

  /**
   * Get available languages
   */
  getAvailableLanguages(): string[] {
    return this.runtimePool.getAvailableLanguages();
  }

  /**
   * Reset all modules
   */
  async reset(): Promise<void> {
    const promises: Promise<void>[] = [];

    for (const language of this.getAvailableLanguages()) {
      const runtime = this.runtimePool.get(language);
      if (runtime) {
        promises.push(runtime.reset());
      }
    }

    await Promise.all(promises);
    this.debugConsole.clear();
  }

  /**
   * Cleanup all modules
   */
  async cleanup(): Promise<void> {
    await this.runtimePool.cleanupAll();
    this.isInitialized = false;
  }

  /**
   * Get profiling data
   */
  getProfilingData() {
    return this.profiler.getProfiles();
  }

  /**
   * Check if initialized
   */
  isReady(): boolean {
    return this.isInitialized;
  }
}

/**
 * Global WASM Module Integrator instance
 */
let globalIntegrator: WasmModuleIntegrator | null = null;

/**
 * Initialize global WASM integrator
 */
export async function initializeWasm(config: ModuleConfig): Promise<WasmModuleIntegrator> {
  if (globalIntegrator) {
    return globalIntegrator;
  }

  globalIntegrator = new WasmModuleIntegrator(config);
  await globalIntegrator.initialize();

  return globalIntegrator;
}

/**
 * Get global WASM integrator
 */
export function getWasmIntegrator(): WasmModuleIntegrator | null {
  return globalIntegrator;
}

/**
 * Singleton pattern helper
 */
export const WasmIntegration = {
  instance: null as WasmModuleIntegrator | null,

  async init(config: ModuleConfig): Promise<WasmModuleIntegrator> {
    return (this.instance = await initializeWasm(config));
  },

  getInstance(): WasmModuleIntegrator | null {
    return this.instance || globalIntegrator;
  },

  async execute(options: ExecutionOptions): Promise<ExecutionResponse> {
    const instance = this.getInstance();
    if (!instance) {
      throw new Error('WASM integration not initialized');
    }
    return instance.execute(options);
  },

  getStats() {
    const instance = this.getInstance();
    if (!instance) {
      return {};
    }
    return instance.getStatistics();
  },

  getDebugReport() {
    const instance = this.getInstance();
    if (!instance) {
      return 'WASM integration not initialized';
    }
    return instance.getDebugReport();
  },

  async cleanup() {
    const instance = this.getInstance();
    if (instance) {
      await instance.cleanup();
    }
  },
};
