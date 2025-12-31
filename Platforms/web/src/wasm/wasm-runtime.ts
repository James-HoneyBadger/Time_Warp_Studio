/**
 * WASM Runtime Environment
 * Manages lifecycle, memory, and execution of WASM interpreters
 * Phase 5.3: WASM Runtime & Integration
 */

export interface WasmExportedFunctions {
  memory: WebAssembly.Memory;
  execute_code: (codePtr: number, codeLen: number) => number;
  cleanup: () => void;
  get_output: (bufPtr: number, bufLen: number) => number;
  get_error: (bufPtr: number, bufLen: number) => number;
  get_graphics_commands: (bufPtr: number, bufLen: number) => number;
  reset_state: () => void;
  set_debug_mode: (enabled: number) => void;
  get_memory_stats: (statsPtr: number) => number;
}

export interface WasmModuleConfig {
  name: string;
  wasmPath: string;
  bufferSize?: number;
  maxExecutionTime?: number;
  enableDebug?: boolean;
}

export interface ExecutionResult {
  success: boolean;
  output: string;
  error?: string;
  graphics?: GraphicsCommand[];
  duration: number;
  memoryUsed: number;
}

export interface GraphicsCommand {
  type: number;
  data: {
    x?: number;
    y?: number;
    distance?: number;
    angle?: number;
    width?: number;
    color?: number;
    text?: string;
  };
}

export interface MemoryStats {
  totalBytes: number;
  usedBytes: number;
  freeBytes: number;
  allocationCount: number;
  executionTime: number;
}

/**
 * WasmRuntime: Core WASM execution environment
 * Handles module loading, memory management, and function binding
 */
export class WasmRuntime {
  private instance: WebAssembly.Instance | null = null;
  private module: WebAssembly.Module | null = null;
  private memory: WebAssembly.Memory | null = null;
  private config: WasmModuleConfig;
  private exports: WasmExportedFunctions | null = null;
  private buffers: Map<string, ArrayBuffer> = new Map();
  private isInitialized: boolean = false;
  private executionStartTime: number = 0;
  private debugMode: boolean = false;

  constructor(config: WasmModuleConfig) {
    this.config = {
      bufferSize: 65536, // 64KB default buffer
      maxExecutionTime: 30000, // 30 second timeout
      enableDebug: false,
      ...config,
    };
    this.debugMode = this.config.enableDebug || false;
  }

  /**
   * Initialize WASM runtime
   * Load module, setup memory, establish function bindings
   */
  async initialize(): Promise<void> {
    if (this.isInitialized) {
      return;
    }

    try {
      this.log(`Initializing WASM runtime for ${this.config.name}`);

      // Fetch WASM module
      const response = await fetch(this.config.wasmPath);
      if (!response.ok) {
        throw new Error(
          `Failed to fetch WASM module: ${response.statusText}`
        );
      }

      const buffer = await response.arrayBuffer();

      // Create WebAssembly module
      this.module = await WebAssembly.compile(buffer);

      // Create memory (256 pages = 16 MB)
      this.memory = new WebAssembly.Memory({
        initial: 256,
        maximum: 512,
        shared: false,
      });

      // Instantiate module with memory import
      const importObject = {
        env: {
          memory: this.memory,
          // JavaScript callbacks for WASM functions
          output_callback: this.onOutput.bind(this),
          error_callback: this.onError.bind(this),
          graphics_callback: this.onGraphics.bind(this),
          input_callback: this.onInput.bind(this),
          log_callback: this.onLog.bind(this),
          debug_callback: this.onDebug.bind(this),
        },
      };

      this.instance = await WebAssembly.instantiate(this.module, importObject);

      // Extract exported functions
      const exp = this.instance.exports as any;
      this.exports = {
        memory: this.memory,
        execute_code: exp.execute_code || (() => 0),
        cleanup: exp.cleanup || (() => {}),
        get_output: exp.get_output || (() => 0),
        get_error: exp.get_error || (() => 0),
        get_graphics_commands: exp.get_graphics_commands || (() => 0),
        reset_state: exp.reset_state || (() => {}),
        set_debug_mode: exp.set_debug_mode || (() => {}),
        get_memory_stats: exp.get_memory_stats || (() => 0),
      };

      // Set debug mode if enabled
      if (this.debugMode && this.exports.set_debug_mode) {
        this.exports.set_debug_mode(1);
      }

      // Allocate buffers
      this.allocateBuffers();

      this.isInitialized = true;
      this.log(`✅ WASM runtime initialized successfully`);
    } catch (error) {
      throw new Error(`WASM initialization failed: ${error}`);
    }
  }

  /**
   * Allocate memory buffers for I/O operations
   */
  private allocateBuffers(): void {
    const bufferSize = this.config.bufferSize || 65536;

    // Code input buffer
    this.buffers.set('code', new ArrayBuffer(bufferSize));

    // Output buffer
    this.buffers.set('output', new ArrayBuffer(bufferSize));

    // Error buffer
    this.buffers.set('error', new ArrayBuffer(bufferSize));

    // Graphics buffer
    this.buffers.set('graphics', new ArrayBuffer(bufferSize));

    // Statistics buffer
    this.buffers.set('stats', new ArrayBuffer(256));

    this.log(`Allocated ${bufferSize} bytes per buffer`);
  }

  /**
   * Execute code in WASM interpreter
   */
  async execute(code: string): Promise<ExecutionResult> {
    if (!this.isInitialized || !this.exports) {
      throw new Error('WASM runtime not initialized');
    }

    // Check timeout
    const timeoutMs = this.config.maxExecutionTime || 30000;
    this.executionStartTime = performance.now();

    try {
      // Encode code as UTF-8
      const encoder = new TextEncoder();
      const encodedCode = encoder.encode(code);

      if (encodedCode.length > (this.config.bufferSize || 65536)) {
        return {
          success: false,
          output: '',
          error: 'Code too large for buffer',
          duration: 0,
          memoryUsed: 0,
        };
      }

      // Write code to WASM memory
      const codeBuffer = this.buffers.get('code');
      if (!codeBuffer) {
        throw new Error('Code buffer not allocated');
      }

      const view = new Uint8Array(
        this.memory!.buffer,
        0,
        encodedCode.length
      );
      view.set(new Uint8Array(encodedCode));

      // Execute
      const resultCode = this.exports.execute_code(0, encodedCode.length);

      // Check for errors
      if (resultCode !== 0) {
        const error = this.readError();
        return {
          success: false,
          output: '',
          error: error || 'Execution failed with error code: ' + resultCode,
          duration: performance.now() - this.executionStartTime,
          memoryUsed: this.getMemoryUsage(),
        };
      }

      // Read results
      const output = this.readOutput();
      const graphics = this.readGraphicsCommands();
      const duration = performance.now() - this.executionStartTime;

      // Check timeout
      if (duration > timeoutMs) {
        this.log(`⚠️  Execution timeout: ${duration}ms > ${timeoutMs}ms`);
      }

      return {
        success: true,
        output,
        graphics,
        duration,
        memoryUsed: this.getMemoryUsage(),
      };
    } catch (error) {
      const duration = performance.now() - this.executionStartTime;
      return {
        success: false,
        output: '',
        error: `Execution error: ${error}`,
        duration,
        memoryUsed: this.getMemoryUsage(),
      };
    }
  }

  /**
   * Read output string from WASM memory
   */
  private readOutput(): string {
    if (!this.exports) return '';

    const outputBuffer = this.buffers.get('output');
    if (!outputBuffer) return '';

    // Call WASM function to populate buffer
    const len = this.exports.get_output(
      0,
      this.config.bufferSize || 65536
    );

    if (len <= 0) return '';

    const view = new Uint8Array(this.memory!.buffer, 0, len);
    const decoder = new TextDecoder();
    return decoder.decode(view);
  }

  /**
   * Read error message from WASM memory
   */
  private readError(): string {
    if (!this.exports) return '';

    const errorBuffer = this.buffers.get('error');
    if (!errorBuffer) return '';

    const len = this.exports.get_error(0, this.config.bufferSize || 65536);

    if (len <= 0) return '';

    const view = new Uint8Array(this.memory!.buffer, 0, len);
    const decoder = new TextDecoder();
    return decoder.decode(view);
  }

  /**
   * Read graphics commands from WASM memory
   */
  private readGraphicsCommands(): GraphicsCommand[] {
    if (!this.exports) return [];

    const graphicsBuffer = this.buffers.get('graphics');
    if (!graphicsBuffer) return [];

    const len = this.exports.get_graphics_commands(
      0,
      this.config.bufferSize || 65536
    );

    if (len <= 0) return [];

    const view = new Uint8Array(this.memory!.buffer, 0, len);
    const decoder = new TextDecoder();
    const data = decoder.decode(view);

    // Parse graphics commands (format: type,x,y,etc\n...)
    const commands: GraphicsCommand[] = [];
    const lines = data.split('\n').filter((l) => l.trim());

    for (const line of lines) {
      const parts = line.split(',');
      if (parts.length < 1) continue;

      const command: GraphicsCommand = {
        type: parseInt(parts[0]),
        data: {},
      };

      // Parse remaining data based on command type
      for (let i = 1; i < parts.length; i += 2) {
        const key = parts[i];
        const value = parts[i + 1];
        if (key && value !== undefined) {
          (command.data as any)[key] = isNaN(+value) ? value : +value;
        }
      }

      commands.push(command);
    }

    return commands;
  }

  /**
   * Get memory usage statistics
   */
  private getMemoryUsage(): number {
    if (!this.exports || !this.memory) return 0;

    const statsBuffer = this.buffers.get('stats');
    if (!statsBuffer) return 0;

    const len = this.exports.get_memory_stats(0);

    if (len <= 0) {
      // Fallback: estimate based on memory buffer
      return this.memory.buffer.byteLength;
    }

    // Parse stats
    const view = new Uint32Array(this.memory.buffer, 0, 8);
    return view[1] || 0; // usedBytes at index 1
  }

  /**
   * Reset interpreter state
   */
  async reset(): Promise<void> {
    if (!this.exports) return;

    try {
      this.exports.reset_state();
      this.log('Interpreter state reset');
    } catch (error) {
      this.log(`Error resetting state: ${error}`);
    }
  }

  /**
   * Cleanup resources
   */
  async cleanup(): Promise<void> {
    if (!this.exports) return;

    try {
      this.exports.cleanup();
      this.isInitialized = false;
      this.log('WASM runtime cleaned up');
    } catch (error) {
      this.log(`Error during cleanup: ${error}`);
    }
  }

  /**
   * Enable/disable debug mode
   */
  setDebugMode(enabled: boolean): void {
    this.debugMode = enabled;
    if (this.exports && this.exports.set_debug_mode) {
      this.exports.set_debug_mode(enabled ? 1 : 0);
    }
  }

  /**
   * Get current memory statistics
   */
  getMemoryStats(): MemoryStats {
    if (!this.memory) {
      return {
        totalBytes: 0,
        usedBytes: 0,
        freeBytes: 0,
        allocationCount: 0,
        executionTime: 0,
      };
    }

    const totalBytes = this.memory.buffer.byteLength;
    const usedBytes = Math.floor(totalBytes * 0.6); // Estimate

    return {
      totalBytes,
      usedBytes,
      freeBytes: totalBytes - usedBytes,
      allocationCount: 0,
      executionTime: performance.now() - this.executionStartTime,
    };
  }

  /**
   * Check if runtime is ready
   */
  isReady(): boolean {
    return this.isInitialized;
  }

  /**
   * Get module name
   */
  getName(): string {
    return this.config.name;
  }

  // ======== WASM Callbacks ========

  private onOutput(ptr: number, len: number): void {
    const view = new Uint8Array(this.memory!.buffer, ptr, len);
    const decoder = new TextDecoder();
    const message = decoder.decode(view);
    this.log(`[OUTPUT] ${message}`);
  }

  private onError(code: number, ptr: number, len: number): void {
    const view = new Uint8Array(this.memory!.buffer, ptr, len);
    const decoder = new TextDecoder();
    const message = decoder.decode(view);
    this.log(`[ERROR] Code ${code}: ${message}`);
  }

  private onGraphics(commandCount: number): void {
    this.log(`[GRAPHICS] ${commandCount} commands generated`);
  }

  private onInput(promptPtr: number, promptLen: number): void {
    const view = new Uint8Array(this.memory!.buffer, promptPtr, promptLen);
    const decoder = new TextDecoder();
    const prompt = decoder.decode(view);
    this.log(`[INPUT] ${prompt}`);
  }

  private onLog(ptr: number, len: number): void {
    const view = new Uint8Array(this.memory!.buffer, ptr, len);
    const decoder = new TextDecoder();
    const message = decoder.decode(view);
    this.log(`[WASM] ${message}`);
  }

  private onDebug(ptr: number, len: number): void {
    if (!this.debugMode) return;
    const view = new Uint8Array(this.memory!.buffer, ptr, len);
    const decoder = new TextDecoder();
    const message = decoder.decode(view);
    console.log(`[DEBUG] ${message}`);
  }

  private log(message: string): void {
    if (this.debugMode) {
      console.log(`[${this.config.name}] ${message}`);
    }
  }
}

/**
 * WasmRuntimePool: Manages multiple WASM runtimes
 * Provides language selection and execution coordination
 */
export class WasmRuntimePool {
  private runtimes: Map<string, WasmRuntime> = new Map();
  private debugMode: boolean = false;

  constructor(debugMode: boolean = false) {
    this.debugMode = debugMode;
  }

  /**
   * Register a WASM runtime
   */
  register(name: string, runtime: WasmRuntime): void {
    this.runtimes.set(name, runtime);
  }

  /**
   * Get runtime by name
   */
  get(name: string): WasmRuntime | undefined {
    return this.runtimes.get(name);
  }

  /**
   * Initialize all runtimes
   */
  async initializeAll(): Promise<void> {
    const promises: Promise<void>[] = [];

    for (const runtime of this.runtimes.values()) {
      if (!runtime.isReady()) {
        promises.push(runtime.initialize());
      }
    }

    await Promise.all(promises);
  }

  /**
   * Execute code with specified language
   */
  async execute(language: string, code: string): Promise<ExecutionResult> {
    const runtime = this.runtimes.get(language);
    if (!runtime) {
      return {
        success: false,
        output: '',
        error: `Unknown language: ${language}`,
        duration: 0,
        memoryUsed: 0,
      };
    }

    if (!runtime.isReady()) {
      await runtime.initialize();
    }

    return runtime.execute(code);
  }

  /**
   * Get all available languages
   */
  getAvailableLanguages(): string[] {
    return Array.from(this.runtimes.keys());
  }

  /**
   * Set debug mode for all runtimes
   */
  setDebugMode(enabled: boolean): void {
    this.debugMode = enabled;
    for (const runtime of this.runtimes.values()) {
      runtime.setDebugMode(enabled);
    }
  }

  /**
   * Cleanup all runtimes
   */
  async cleanupAll(): Promise<void> {
    const promises: Promise<void>[] = [];

    for (const runtime of this.runtimes.values()) {
      promises.push(runtime.cleanup());
    }

    await Promise.all(promises);
  }
}
