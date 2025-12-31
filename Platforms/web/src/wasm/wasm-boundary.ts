/**
 * WASM JavaScript Boundary Layer
 * Provides safe JavaScript ↔ WASM communication and memory management
 * Phase 5.3: WASM Runtime & Integration
 */

export interface WasmBridge {
  call(functionName: string, ...args: any[]): any;
  readMemory(offset: number, length: number): Uint8Array;
  writeMemory(offset: number, data: Uint8Array): number;
  allocateMemory(size: number): number;
  freeMemory(offset: number): void;
  getMemoryBuffer(): ArrayBuffer;
  getMemoryView(offset: number, length: number): DataView;
}

export interface BoundaryConfig {
  maxCallDepth?: number;
  enableStackTrace?: boolean;
  enableProfiling?: boolean;
  memoryPoolSize?: number;
}

/**
 * WasmMemoryPool: Manages memory allocation in WASM linear memory
 */
export class WasmMemoryPool {
  private heap: Uint8Array;
  private allocations: Map<number, number> = new Map(); // offset -> size
  private freeBlocks: Array<{ offset: number; size: number }> = [];
  private heapStart: number = 0;
  private heapEnd: number = 0;
  private nextAlloc: number = 0;

  constructor(buffer: ArrayBuffer, heapStart: number = 0, heapSize: number = 1024 * 1024) {
    this.heap = new Uint8Array(buffer);
    this.heapStart = heapStart;
    this.heapEnd = heapStart + heapSize;
    this.nextAlloc = heapStart;
  }

  /**
   * Allocate memory block
   */
  allocate(size: number): number {
    if (size <= 0) {
      throw new Error('Invalid allocation size');
    }

    // Try to find free block
    for (let i = 0; i < this.freeBlocks.length; i++) {
      const block = this.freeBlocks[i];
      if (block.size >= size) {
        const offset = block.offset;
        
        // Split block if larger than needed
        if (block.size > size) {
          block.offset += size;
          block.size -= size;
        } else {
          this.freeBlocks.splice(i, 1);
        }

        this.allocations.set(offset, size);
        return offset;
      }
    }

    // Allocate from heap
    if (this.nextAlloc + size > this.heapEnd) {
      throw new Error('Heap exhausted');
    }

    const offset = this.nextAlloc;
    this.nextAlloc += size;
    this.allocations.set(offset, size);

    return offset;
  }

  /**
   * Free allocated memory
   */
  free(offset: number): void {
    const size = this.allocations.get(offset);
    if (size === undefined) {
      throw new Error('Invalid memory offset');
    }

    this.allocations.delete(offset);

    // Add to free blocks
    this.freeBlocks.push({ offset, size });

    // Merge adjacent free blocks
    this.mergeFreeBlocks();
  }

  /**
   * Merge adjacent free blocks
   */
  private mergeFreeBlocks(): void {
    this.freeBlocks.sort((a, b) => a.offset - b.offset);

    for (let i = 0; i < this.freeBlocks.length - 1; i++) {
      const current = this.freeBlocks[i];
      const next = this.freeBlocks[i + 1];

      if (current.offset + current.size === next.offset) {
        current.size += next.size;
        this.freeBlocks.splice(i + 1, 1);
        i--;
      }
    }
  }

  /**
   * Get memory statistics
   */
  getStats() {
    const totalAllocated = Array.from(this.allocations.values()).reduce((a, b) => a + b, 0);
    const totalFree = this.freeBlocks.reduce((a, b) => a + b.size, 0);

    return {
      totalBytes: this.heapEnd - this.heapStart,
      allocatedBytes: totalAllocated,
      freeBytes: totalFree,
      allocationCount: this.allocations.size,
      fragmentationRatio: totalFree > 0 ? this.freeBlocks.length / (totalAllocated + totalFree) : 0,
    };
  }

  /**
   * Write data to memory
   */
  writeData(offset: number, data: Uint8Array): void {
    const size = this.allocations.get(offset);
    if (size === undefined) {
      throw new Error('Invalid memory offset');
    }

    if (data.length > size) {
      throw new Error('Data too large for allocation');
    }

    this.heap.set(data, offset);
  }

  /**
   * Read data from memory
   */
  readData(offset: number, length: number): Uint8Array {
    return this.heap.slice(offset, offset + length);
  }

  /**
   * Clear all allocations
   */
  clear(): void {
    this.allocations.clear();
    this.freeBlocks = [];
    this.nextAlloc = this.heapStart;
  }
}

/**
 * WasmBridgeImpl: JavaScript boundary for WASM modules
 * Provides safe calling conventions and memory management
 */
export class WasmBridgeImpl implements WasmBridge {
  private instance: WebAssembly.Instance;
  private memory: WebAssembly.Memory;
  private memoryPool: WasmMemoryPool;
  private callStack: string[] = [];
  private config: BoundaryConfig;
  private callTimes: Map<string, number[]> = new Map();

  constructor(
    instance: WebAssembly.Instance,
    memory: WebAssembly.Memory,
    config: BoundaryConfig = {}
  ) {
    this.instance = instance;
    this.memory = memory;
    this.config = {
      maxCallDepth: 100,
      enableStackTrace: false,
      enableProfiling: false,
      ...config,
    };
    this.memoryPool = new WasmMemoryPool(memory.buffer, 0, 1024 * 1024);
  }

  /**
   * Call WASM function with safety checks
   */
  call(functionName: string, ...args: any[]): any {
    const startTime = performance.now();

    // Check call depth
    if (this.callStack.length >= (this.config.maxCallDepth || 100)) {
      throw new Error('Maximum call depth exceeded');
    }

    // Push to call stack
    this.callStack.push(functionName);

    try {
      const fn = (this.instance.exports as any)[functionName];
      if (typeof fn !== 'function') {
        throw new Error(`Function not found: ${functionName}`);
      }

      // Call function
      const result = fn(...args);

      // Record timing if profiling enabled
      if (this.config.enableProfiling) {
        if (!this.callTimes.has(functionName)) {
          this.callTimes.set(functionName, []);
        }
        const duration = performance.now() - startTime;
        this.callTimes.get(functionName)!.push(duration);
      }

      return result;
    } finally {
      this.callStack.pop();
    }
  }

  /**
   * Read memory
   */
  readMemory(offset: number, length: number): Uint8Array {
    return this.memoryPool.readData(offset, length);
  }

  /**
   * Write memory
   */
  writeMemory(offset: number, data: Uint8Array): number {
    this.memoryPool.writeData(offset, data);
    return data.length;
  }

  /**
   * Allocate memory
   */
  allocateMemory(size: number): number {
    return this.memoryPool.allocate(size);
  }

  /**
   * Free memory
   */
  freeMemory(offset: number): void {
    this.memoryPool.free(offset);
  }

  /**
   * Get memory buffer
   */
  getMemoryBuffer(): ArrayBuffer {
    return this.memory.buffer;
  }

  /**
   * Get memory data view
   */
  getMemoryView(offset: number, length: number): DataView {
    return new DataView(this.memory.buffer, offset, length);
  }

  /**
   * Get call stack trace
   */
  getStackTrace(): string[] {
    return [...this.callStack];
  }

  /**
   * Get profiling data
   */
  getProfilingData(): Record<string, { calls: number; totalTime: number; avgTime: number }> {
    const data: Record<string, { calls: number; totalTime: number; avgTime: number }> = {};

    for (const [name, times] of this.callTimes.entries()) {
      const totalTime = times.reduce((a, b) => a + b, 0);
      data[name] = {
        calls: times.length,
        totalTime,
        avgTime: totalTime / times.length,
      };
    }

    return data;
  }

  /**
   * Get memory statistics
   */
  getMemoryStats() {
    return this.memoryPool.getStats();
  }

  /**
   * Reset profiling data
   */
  resetProfiling(): void {
    this.callTimes.clear();
  }

  /**
   * Reset memory
   */
  resetMemory(): void {
    this.memoryPool.clear();
  }
}

/**
 * Boundary Module: Provides shared utilities for WASM interaction
 */
export class BoundaryModule {
  /**
   * Encode string as UTF-8 bytes in WASM memory
   */
  static encodeString(text: string): Uint8Array {
    const encoder = new TextEncoder();
    return encoder.encode(text);
  }

  /**
   * Decode UTF-8 bytes from WASM memory
   */
  static decodeString(bytes: Uint8Array): string {
    const decoder = new TextDecoder();
    return decoder.decode(bytes);
  }

  /**
   * Convert JavaScript value to WASM-compatible format
   */
  static toWasmValue(value: any): number {
    if (typeof value === 'number') {
      return value;
    }
    if (typeof value === 'boolean') {
      return value ? 1 : 0;
    }
    if (value === null || value === undefined) {
      return 0;
    }
    throw new Error(`Cannot convert ${typeof value} to WASM value`);
  }

  /**
   * Convert WASM value to JavaScript
   */
  static fromWasmValue(value: number, type: 'number' | 'boolean' | 'void'): any {
    switch (type) {
      case 'number':
        return value;
      case 'boolean':
        return value !== 0;
      case 'void':
        return undefined;
      default:
        return value;
    }
  }

  /**
   * Safely call WASM function with error handling
   */
  static async safeCall(
    fn: Function,
    ...args: any[]
  ): Promise<{ success: boolean; result?: any; error?: string }> {
    try {
      const result = await Promise.resolve(fn(...args));
      return { success: true, result };
    } catch (error) {
      return { success: false, error: String(error) };
    }
  }

  /**
   * Create memory wrapper for safe access
   */
  static createMemoryWrapper(buffer: ArrayBuffer, offset: number, size: number) {
    return {
      buffer: new Uint8Array(buffer, offset, size),
      view: new DataView(buffer, offset, size),
      readString(encoding: BufferEncoding = 'utf-8'): string {
        const decoder = new TextDecoder(encoding);
        return decoder.decode(this.buffer);
      },
      writeString(text: string): void {
        const encoder = new TextEncoder();
        const encoded = encoder.encode(text);
        if (encoded.length > size) {
          throw new Error('String too large for buffer');
        }
        this.buffer.set(encoded);
      },
      readUint8(index: number): number {
        return this.buffer[index];
      },
      writeUint8(index: number, value: number): void {
        this.buffer[index] = value;
      },
      readUint32(offset: number): number {
        return this.view.getUint32(offset, true);
      },
      writeUint32(offset: number, value: number): void {
        this.view.setUint32(offset, value, true);
      },
      readFloat32(offset: number): number {
        return this.view.getFloat32(offset, true);
      },
      writeFloat32(offset: number, value: number): void {
        this.view.setFloat32(offset, value, true);
      },
    };
  }
}

/**
 * Error boundary for WASM execution
 */
export class WasmErrorBoundary {
  private errors: Array<{ message: string; stack: string; timestamp: number }> = [];
  private maxErrors: number = 100;

  /**
   * Report error
   */
  reportError(error: Error, context?: string): void {
    this.errors.push({
      message: error.message,
      stack: context ? `${context}\n${error.stack}` : error.stack || '',
      timestamp: Date.now(),
    });

    // Trim old errors
    if (this.errors.length > this.maxErrors) {
      this.errors = this.errors.slice(-this.maxErrors);
    }

    console.error(`[WASM Error] ${error.message}`, context);
  }

  /**
   * Get all errors
   */
  getErrors() {
    return [...this.errors];
  }

  /**
   * Clear errors
   */
  clear(): void {
    this.errors = [];
  }

  /**
   * Create error boundary wrapper
   */
  wrap<T extends any[], R>(fn: (...args: T) => R, context?: string) {
    return (...args: T): R | null => {
      try {
        return fn(...args);
      } catch (error) {
        this.reportError(error as Error, context);
        return null;
      }
    };
  }
}
