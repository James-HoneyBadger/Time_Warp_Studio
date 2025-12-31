/**
 * WASM Loader & Runtime
 * Manages WebAssembly module loading and execution for Time Warp IDE
 * 
 * Architecture:
 * - WasmLoader: Loads .wasm modules and initializes instances
 * - WasmMemory: Manages linear memory and string conversion
 * - WasmInterpreter: High-level interface for code execution
 * - WasmBridge: JavaScript callbacks for WASM interaction
 */

import { logger } from '../utils/logger';
import type { ExecutionResult, OutputCallback } from '../types';

// WASM Instance Type
interface WasmInstance {
  memory: WebAssembly.Memory;
  execute_code: (codePtr: number, codeLen: number) => number;
  init_interpreter: () => void;
  cleanup: () => void;
  get_error: (ptr: number) => number;
  get_output: (ptr: number) => number;
  malloc: (size: number) => number;
  free: (ptr: number) => void;
}

interface WasmModule {
  instance: WasmInstance;
  module: WebAssembly.Module;
}

/**
 * WASM Memory Manager
 * Handles allocation, deallocation, and string conversion
 */
class WasmMemory {
  private instance: WasmInstance;
  private buffer: DataView;

  constructor(instance: WasmInstance) {
    this.instance = instance;
    this.buffer = new DataView(instance.memory.buffer);
  }

  /**
   * Allocate memory in WASM
   */
  allocate(size: number): number {
    return this.instance.malloc(size);
  }

  /**
   * Deallocate memory in WASM
   */
  deallocate(ptr: number): void {
    this.instance.free(ptr);
  }

  /**
   * Write string to WASM memory
   */
  writeString(str: string): number {
    const encoder = new TextEncoder();
    const bytes = encoder.encode(str);
    const ptr = this.allocate(bytes.length + 1);

    const view = new Uint8Array(this.instance.memory.buffer, ptr, bytes.length);
    view.set(bytes);
    // Null terminate
    this.buffer.setUint8(ptr + bytes.length, 0);

    return ptr;
  }

  /**
   * Read string from WASM memory
   */
  readString(ptr: number): string {
    const bytes: number[] = [];
    let offset = 0;

    while (true) {
      const byte = this.buffer.getUint8(ptr + offset);
      if (byte === 0) break;
      bytes.push(byte);
      offset++;
    }

    const decoder = new TextDecoder();
    return decoder.decode(new Uint8Array(bytes));
  }

  /**
   * Update buffer reference (after memory growth)
   */
  updateBuffer(): void {
    this.buffer = new DataView(this.instance.memory.buffer);
  }
}

/**
 * WASM Module Loader
 * Loads and initializes WASM modules
 */
export class WasmLoader {
  private modules: Map<string, WasmModule> = new Map();
  private wasmPath: string = '/wasm';

  constructor(wasmPath: string = '/wasm') {
    this.wasmPath = wasmPath;
  }

  /**
   * Load a WASM module
   * @param language Language name (basic, logo, pilot, etc)
   * @returns Promise resolving to WasmInstance
   */
  async loadModule(language: string): Promise<WasmInstance> {
    // Check cache
    if (this.modules.has(language)) {
      const cached = this.modules.get(language)!;
      // Re-initialize for fresh state
      cached.instance.init_interpreter();
      return cached.instance;
    }

    try {
      logger.info(`Loading WASM module: ${language}`);

      // Fetch module
      const wasmPath = `${this.wasmPath}/${language}.wasm`;
      const response = await fetch(wasmPath);

      if (!response.ok) {
        throw new Error(`Failed to load WASM: ${response.statusText}`);
      }

      const buffer = await response.arrayBuffer();

      // Create memory
      const memory = new WebAssembly.Memory({
        initial: 256, // 256 pages = 16 MB
        maximum: 512, // 512 pages = 32 MB
        shared: false
      });

      // Create import object
      const imports = {
        env: {
          memory,
          // Import functions can be added here
          // e.g., console output, graphics callbacks
        }
      };

      // Instantiate module
      const wasmModule = await WebAssembly.instantiate(
        buffer,
        imports
      );

      const instance = wasmModule.instance.exports as unknown as WasmInstance;

      // Store in cache
      this.modules.set(language, {
        instance,
        module: wasmModule.module
      });

      // Initialize interpreter
      instance.init_interpreter();

      logger.info(`WASM module loaded: ${language}`);
      return instance;

    } catch (error) {
      logger.error(`Failed to load WASM module ${language}:`, error);
      throw error;
    }
  }

  /**
   * Unload a WASM module
   */
  unloadModule(language: string): void {
    if (this.modules.has(language)) {
      const module = this.modules.get(language)!;
      module.instance.cleanup();
      this.modules.delete(language);
      logger.info(`WASM module unloaded: ${language}`);
    }
  }

  /**
   * Check if module is loaded
   */
  isLoaded(language: string): boolean {
    return this.modules.has(language);
  }

  /**
   * Get loaded modules
   */
  getLoadedModules(): string[] {
    return Array.from(this.modules.keys());
  }
}

/**
 * WASM Interpreter
 * High-level interface for executing code in WASM
 */
export class WasmInterpreter {
  private loader: WasmLoader;
  private instance: WasmInstance | null = null;
  private memory: WasmMemory | null = null;
  private language: string = '';
  private isInitialized = false;

  constructor(wasmPath?: string) {
    this.loader = new WasmLoader(wasmPath);
  }

  /**
   * Initialize interpreter for a language
   */
  async initialize(language: string): Promise<void> {
    try {
      this.language = language;
      this.instance = await this.loader.loadModule(language);
      this.memory = new WasmMemory(this.instance);
      this.isInitialized = true;

      logger.info(`${language} interpreter initialized`);
    } catch (error) {
      this.isInitialized = false;
      throw error;
    }
  }

  /**
   * Execute code in WASM
   */
  async execute(code: string): Promise<ExecutionResult> {
    if (!this.isInitialized || !this.instance || !this.memory) {
      throw new Error('Interpreter not initialized');
    }

    try {
      const startTime = performance.now();

      // Write code to WASM memory
      const codePtr = this.memory.writeString(code);

      // Execute
      const resultCode = this.instance.execute_code(codePtr, code.length);

      // Get output
      const outputPtr = this.instance.get_output(0);
      const output = this.memory.readString(outputPtr);

      // Get error (if any)
      let error = '';
      if (resultCode !== 0) {
        const errorPtr = this.instance.get_error(0);
        error = this.memory.readString(errorPtr);
      }

      // Cleanup
      this.memory.deallocate(codePtr);

      const duration = performance.now() - startTime;

      return {
        success: resultCode === 0,
        output,
        error,
        duration,
        isWasm: true
      };

    } catch (error) {
      return {
        success: false,
        output: '',
        error: String(error),
        duration: 0,
        isWasm: true
      };
    }
  }

  /**
   * Reset interpreter state
   */
  async reset(): Promise<void> {
    if (this.instance) {
      this.instance.cleanup();
      this.instance.init_interpreter();
      logger.info(`${this.language} interpreter reset`);
    }
  }

  /**
   * Cleanup and shutdown
   */
  async shutdown(): Promise<void> {
    if (this.instance) {
      this.instance.cleanup();
    }
    if (this.language) {
      this.loader.unloadModule(this.language);
    }
    this.instance = null;
    this.memory = null;
    this.isInitialized = false;
  }

  /**
   * Get interpreter status
   */
  getStatus() {
    return {
      language: this.language,
      isInitialized: this.isInitialized,
      loadedModules: this.loader.getLoadedModules()
    };
  }
}

/**
 * WASM Bridge
 * Manages multiple language interpreters and fallback to server
 */
export class WasmBridge {
  private interpreters: Map<string, WasmInterpreter> = new Map();
  private fallbackToServer: boolean = true;
  private currentLanguage: string = '';

  constructor(fallbackToServer: boolean = true) {
    this.fallbackToServer = fallbackToServer;
  }

  /**
   * Get or create interpreter for language
   */
  async getInterpreter(language: string): Promise<WasmInterpreter | null> {
    try {
      if (!this.interpreters.has(language)) {
        const interpreter = new WasmInterpreter();
        await interpreter.initialize(language);
        this.interpreters.set(language, interpreter);
      }
      return this.interpreters.get(language)!;
    } catch (error) {
      logger.warn(`Failed to load WASM interpreter for ${language}:`, error);
      if (this.fallbackToServer) {
        logger.info(`Falling back to server execution for ${language}`);
        return null;
      }
      throw error;
    }
  }

  /**
   * Execute code with WASM or fallback
   */
  async execute(
    language: string,
    code: string
  ): Promise<ExecutionResult> {
    try {
      const interpreter = await this.getInterpreter(language);

      if (interpreter) {
        // Execute in WASM
        return await interpreter.execute(code);
      } else if (this.fallbackToServer) {
        // Fallback to server
        return {
          success: false,
          output: '',
          error: 'Falling back to server execution',
          duration: 0,
          isWasm: false,
          fallback: true
        };
      }
    } catch (error) {
      logger.error(`Execution error for ${language}:`, error);
      throw error;
    }

    throw new Error(`No interpreter available for ${language}`);
  }

  /**
   * Preload all interpreters
   */
  async preloadAll(languages: string[]): Promise<void> {
    const results = await Promise.allSettled(
      languages.map(lang => this.getInterpreter(lang))
    );

    const loaded = results.filter(r => r.status === 'fulfilled').length;
    logger.info(`Preloaded ${loaded}/${languages.length} interpreters`);
  }

  /**
   * Cleanup all interpreters
   */
  async cleanup(): Promise<void> {
    for (const [language, interpreter] of this.interpreters) {
      try {
        await interpreter.shutdown();
      } catch (error) {
        logger.error(`Error shutting down ${language} interpreter:`, error);
      }
    }
    this.interpreters.clear();
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      loadedInterpreters: Array.from(this.interpreters.keys()),
      fallbackEnabled: this.fallbackToServer,
      count: this.interpreters.size
    };
  }
}

// Singleton instance
let wasmBridge: WasmBridge | null = null;

/**
 * Get or create WASM bridge instance
 */
export function getWasmBridge(
  fallbackToServer: boolean = true
): WasmBridge {
  if (!wasmBridge) {
    wasmBridge = new WasmBridge(fallbackToServer);
  }
  return wasmBridge;
}

/**
 * Check WASM support
 */
export function isWasmSupported(): boolean {
  return typeof WebAssembly !== 'undefined';
}

/**
 * Preload WASM modules on app startup
 */
export async function preloadWasmModules(
  languages: string[] = ['basic', 'logo', 'pilot']
): Promise<void> {
  if (!isWasmSupported()) {
    logger.warn('WebAssembly not supported in this browser');
    return;
  }

  try {
    const bridge = getWasmBridge();
    await bridge.preloadAll(languages);
    logger.info('WASM modules preloaded successfully');
  } catch (error) {
    logger.error('Failed to preload WASM modules:', error);
  }
}
