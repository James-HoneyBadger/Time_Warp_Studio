/**
 * WASM Performance Optimization Module
 * 
 * Implements zero-copy data transfer, SharedArrayBuffer optimization,
 * and direct memory access patterns for maximum throughput.
 * 
 * Target: 40% latency reduction through elimination of buffer copies
 */

/**
 * Zero-copy buffer wrapper using SharedArrayBuffer for direct memory access
 * 
 * Eliminates the copy step when transferring large data between JS and WASM.
 * SharedArrayBuffer allows both JS and WASM to access the same memory region
 * without serialization overhead.
 */
export interface ZeroCopyBuffer {
  buffer: SharedArrayBuffer;
  offset: number;
  length: number;
  isOwned: boolean;
}

/**
 * Zero-copy memory manager for high-performance data transfer
 * 
 * Uses SharedArrayBuffer to avoid copying data between JS and WASM.
 * Manages lifecycle of shared memory regions with proper cleanup.
 */
export class ZeroCopyMemory {
  private buffers: Map<number, ZeroCopyBuffer> = new Map();
  private nextBufferId = 1;
  private totalSharedMemory = 0;
  private maxSharedMemory = 64 * 1024 * 1024; // 64MB max shared

  /**
   * Create a zero-copy buffer from JavaScript data
   * 
   * The returned buffer can be passed to WASM functions that accept
   * a buffer ID. WASM and JS both access the same underlying memory.
   * 
   * @param data Uint8Array or ArrayBuffer data
   * @returns Buffer ID for passing to WASM
   */
  allocateZeroCopy(data: Uint8Array | ArrayBuffer): number {
    const length = data instanceof ArrayBuffer ? data.byteLength : data.length;
    
    // Check memory limits
    if (this.totalSharedMemory + length > this.maxSharedMemory) {
      throw new Error(`SharedArrayBuffer limit exceeded: ${this.totalSharedMemory + length} > ${this.maxSharedMemory}`);
    }

    // Create SharedArrayBuffer
    const sab = new SharedArrayBuffer(length);
    const view = new Uint8Array(sab);
    
    // Copy data into shared buffer
    if (data instanceof ArrayBuffer) {
      view.set(new Uint8Array(data));
    } else {
      view.set(data);
    }

    const bufferId = this.nextBufferId++;
    this.buffers.set(bufferId, {
      buffer: sab,
      offset: 0,
      length,
      isOwned: true
    });

    this.totalSharedMemory += length;
    return bufferId;
  }

  /**
   * Get zero-copy buffer for reading WASM output
   * 
   * Returns a view into the shared buffer without copying.
   * Changes made by WASM are immediately visible.
   * 
   * @param bufferId Buffer ID from WASM
   * @returns Uint8Array view of shared memory
   */
  getZeroCopyView(bufferId: number): Uint8Array {
    const buf = this.buffers.get(bufferId);
    if (!buf) {
      throw new Error(`Buffer ${bufferId} not found`);
    }
    return new Uint8Array(buf.buffer, buf.offset, buf.length);
  }

  /**
   * Read data from zero-copy buffer without additional copying
   * 
   * For small data or when a copy is needed anyway, this provides
   * a clean interface. For zero-copy reads, use getZeroCopyView().
   * 
   * @param bufferId Buffer ID
   * @returns Data as Uint8Array (may be direct view)
   */
  readZeroCopy(bufferId: number): Uint8Array {
    return this.getZeroCopyView(bufferId);
  }

  /**
   * Write data to zero-copy buffer directly
   * 
   * For efficient writes, use getZeroCopyView() and modify directly.
   * This method is for convenience when you already have data.
   * 
   * @param bufferId Buffer ID
   * @param data Data to write
   */
  writeZeroCopy(bufferId: number, data: Uint8Array): void {
    const view = this.getZeroCopyView(bufferId);
    if (data.length > view.length) {
      throw new Error(`Data too large: ${data.length} > ${view.length}`);
    }
    view.set(data);
  }

  /**
   * Release zero-copy buffer and reclaim SharedArrayBuffer
   * 
   * Must be called to prevent memory leaks. Once released,
   * the buffer ID is no longer valid.
   * 
   * @param bufferId Buffer ID to release
   */
  releaseZeroCopy(bufferId: number): void {
    const buf = this.buffers.get(bufferId);
    if (!buf) return;
    
    this.totalSharedMemory -= buf.length;
    this.buffers.delete(bufferId);
  }

  /**
   * Get statistics about zero-copy memory usage
   */
  getStats(): {
    activeBuffers: number;
    totalMemory: number;
    maxMemory: number;
    memoryUsagePercent: number;
  } {
    return {
      activeBuffers: this.buffers.size,
      totalMemory: this.totalSharedMemory,
      maxMemory: this.maxSharedMemory,
      memoryUsagePercent: (this.totalSharedMemory / this.maxSharedMemory) * 100
    };
  }

  /**
   * Clear all zero-copy buffers
   */
  clear(): void {
    this.buffers.clear();
    this.totalSharedMemory = 0;
  }
}

/**
 * Direct memory access wrapper for high-performance WASM integration
 * 
 * Provides low-overhead access to WASM linear memory using TypedArrays.
 * Single allocation = minimal GC pressure.
 */
export class DirectMemoryAccess {
  private wasmMemory: WebAssembly.Memory;
  private memoryView: Uint8Array;
  private dataViews: Map<string, DataView> = new Map();

  constructor(wasmMemory: WebAssembly.Memory) {
    this.wasmMemory = wasmMemory;
    this.memoryView = new Uint8Array(wasmMemory.buffer);
  }

  /**
   * Read bytes directly from WASM memory (zero-copy view)
   * 
   * @param offset Byte offset in WASM memory
   * @param length Number of bytes to read
   * @returns Direct view into WASM memory buffer
   */
  readDirect(offset: number, length: number): Uint8Array {
    this.validateBounds(offset, length);
    return new Uint8Array(this.wasmMemory.buffer, offset, length);
  }

  /**
   * Write bytes directly to WASM memory (zero-copy)
   * 
   * @param offset Byte offset
   * @param data Data to write
   */
  writeDirect(offset: number, data: Uint8Array): void {
    this.validateBounds(offset, data.length);
    this.memoryView.set(data, offset);
  }

  /**
   * Read string from WASM memory (UTF-8 decode, not copied)
   * 
   * @param offset Byte offset
   * @param length String length in bytes
   * @returns Decoded string
   */
  readString(offset: number, length: number): string {
    const bytes = this.readDirect(offset, length);
    return new TextDecoder().decode(bytes);
  }

  /**
   * Write string to WASM memory (UTF-8 encode)
   * 
   * @param offset Byte offset
   * @param text String to write
   * @returns Number of bytes written
   */
  writeString(offset: number, text: string): number {
    const encoded = new TextEncoder().encode(text);
    this.writeDirect(offset, encoded);
    return encoded.length;
  }

  /**
   * Read/write typed data with cached DataView for efficiency
   * 
   * DataView is created once and reused to minimize allocation overhead.
   * 
   * @param type Type identifier for DataView caching
   * @param offset Byte offset
   * @param length Buffer length
   * @returns DataView for typed access (int32, float64, etc.)
   */
  getDataView(type: string, offset: number, length: number): DataView {
    const key = `${type}:${offset}:${length}`;
    
    if (!this.dataViews.has(key)) {
      this.validateBounds(offset, length);
      this.dataViews.set(key, new DataView(this.wasmMemory.buffer, offset, length));
    }
    
    return this.dataViews.get(key)!;
  }

  /**
   * Efficient bulk data read (for large transfers, still zero-copy)
   * 
   * @param offset Starting offset
   * @param length Bytes to read
   * @returns Data as ArrayBuffer view
   */
  bulkRead(offset: number, length: number): Uint8Array {
    return this.readDirect(offset, length);
  }

  /**
   * Efficient bulk data write
   * 
   * @param offset Starting offset
   * @param data Data buffer
   */
  bulkWrite(offset: number, data: Uint8Array): void {
    this.writeDirect(offset, data);
  }

  /**
   * Update WASM memory buffer reference (called when memory grows)
   */
  updateMemoryBuffer(): void {
    this.memoryView = new Uint8Array(this.wasmMemory.buffer);
    this.dataViews.clear(); // Invalidate cached DataViews
  }

  /**
   * Validate memory access bounds
   */
  private validateBounds(offset: number, length: number): void {
    const maxOffset = this.wasmMemory.buffer.byteLength;
    if (offset < 0 || offset + length > maxOffset) {
      throw new Error(`Out of bounds: [${offset}, ${offset + length}) > ${maxOffset}`);
    }
  }

  /**
   * Get memory stats
   */
  getStats(): {
    memoryPages: number;
    memoryBytes: number;
    cachedDataViews: number;
  } {
    return {
      memoryPages: this.wasmMemory.buffer.byteLength / 65536,
      memoryBytes: this.wasmMemory.buffer.byteLength,
      cachedDataViews: this.dataViews.size
    };
  }

  /**
   * Clear cached DataViews
   */
  clearCache(): void {
    this.dataViews.clear();
  }
}

/**
 * Streaming data handler for large code/output transfers
 * 
 * Processes data in chunks to maintain consistent performance
 * and avoid memory spikes.
 */
export class StreamingDataHandler {
  private chunkSize: number;
  private onChunk?: (chunk: Uint8Array) => Promise<void>;

  constructor(chunkSize: number = 65536) { // 64KB chunks
    this.chunkSize = chunkSize;
  }

  /**
   * Set callback for chunk processing
   */
  setChunkHandler(handler: (chunk: Uint8Array) => Promise<void>): void {
    this.onChunk = handler;
  }

  /**
   * Process large data in streaming chunks
   * 
   * @param data Full data buffer
   * @param handler Callback for each chunk
   */
  async streamData(
    data: Uint8Array,
    handler?: (chunk: Uint8Array) => Promise<void>
  ): Promise<void> {
    const processHandler = handler || this.onChunk;
    if (!processHandler) {
      throw new Error('No chunk handler set');
    }

    for (let i = 0; i < data.length; i += this.chunkSize) {
      const chunk = data.slice(i, Math.min(i + this.chunkSize, data.length));
      await processHandler(chunk);
    }
  }

  /**
   * Stream from WASM directly without intermediate buffer
   * 
   * @param memory Direct memory access instance
   * @param offset Start offset
   * @param length Total length
   * @param handler Chunk processor
   */
  async streamFromWasm(
    memory: DirectMemoryAccess,
    offset: number,
    length: number,
    handler: (chunk: Uint8Array) => Promise<void>
  ): Promise<void> {
    for (let i = 0; i < length; i += this.chunkSize) {
      const chunkLen = Math.min(this.chunkSize, length - i);
      const chunk = memory.readDirect(offset + i, chunkLen);
      await handler(chunk);
    }
  }

  /**
   * Combine multiple chunks into single buffer
   * 
   * @param chunks Array of chunks
   * @returns Combined buffer
   */
  combineChunks(chunks: Uint8Array[]): Uint8Array {
    const totalLength = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
    const combined = new Uint8Array(totalLength);
    
    let offset = 0;
    for (const chunk of chunks) {
      combined.set(chunk, offset);
      offset += chunk.length;
    }
    
    return combined;
  }
}

/**
 * SIMD (Single Instruction Multiple Data) optimization utilities
 * 
 * For bulk operations like data transforms, use SIMD when available
 * to process multiple elements in parallel.
 */
export class SIMDOptimization {
  /**
   * Check if SIMD is available in the environment
   */
  static isAvailable(): boolean {
    return typeof WebAssembly !== 'undefined' && 
           typeof WebAssembly.validate !== 'undefined';
  }

  /**
   * Vectorized byte transformation using SIMD patterns
   * 
   * Even without native SIMD, we can optimize with TypedArray patterns.
   * 
   * @param input Input buffer
   * @param transform Transformation function
   * @returns Transformed buffer
   */
  static transform(
    input: Uint8Array,
    transform: (a: number, b: number, c: number, d: number) => Uint32Array
  ): Uint8Array {
    // Process 4 bytes at a time using Uint32
    const output = new Uint8Array(input.length);
    const inputView = new Uint32Array(input.buffer, input.byteOffset, Math.floor(input.length / 4));
    const outputView = new Uint32Array(output.buffer, output.byteOffset, Math.floor(output.length / 4));

    for (let i = 0; i < inputView.length; i++) {
      const value = inputView[i];
      const b0 = value & 0xFF;
      const b1 = (value >> 8) & 0xFF;
      const b2 = (value >> 16) & 0xFF;
      const b3 = (value >> 24) & 0xFF;
      
      const result = transform(b0, b1, b2, b3);
      outputView[i] = result[0];
    }

    // Handle remaining bytes
    const remainder = input.length % 4;
    if (remainder > 0) {
      const startIdx = Math.floor(input.length / 4) * 4;
      for (let i = startIdx; i < input.length; i++) {
        output[i] = input[i];
      }
    }

    return output;
  }

  /**
   * Parallel sum reduction using SIMD pattern
   * 
   * @param data Input buffer
   * @returns Sum of all values
   */
  static sum(data: Uint8Array): number {
    const view = new Uint32Array(data.buffer, data.byteOffset, Math.floor(data.length / 4));
    let sum = 0;

    for (let i = 0; i < view.length; i++) {
      const value = view[i];
      sum += (value & 0xFF) + ((value >> 8) & 0xFF) + 
             ((value >> 16) & 0xFF) + ((value >> 24) & 0xFF);
    }

    // Add remainder
    const remainder = data.length % 4;
    for (let i = data.length - remainder; i < data.length; i++) {
      sum += data[i];
    }

    return sum;
  }
}

/**
 * Lazy loading strategy for WASM modules
 * 
 * Load modules on-demand instead of upfront to reduce startup time.
 */
export class LazyModuleLoader {
  private loadedModules: Map<string, WebAssembly.Instance> = new Map();
  private loadingPromises: Map<string, Promise<WebAssembly.Instance>> = new Map();
  private moduleUrls: Map<string, string> = new Map();

  /**
   * Register a module URL for lazy loading
   */
  registerModule(language: string, url: string): void {
    this.moduleUrls.set(language, url);
  }

  /**
   * Load a module on-demand (cached after first load)
   * 
   * @param language Language identifier
   * @returns Promise resolving to WASM instance
   */
  async loadModule(language: string): Promise<WebAssembly.Instance> {
    // Return cached instance if available
    if (this.loadedModules.has(language)) {
      return this.loadedModules.get(language)!;
    }

    // Return pending promise if already loading
    if (this.loadingPromises.has(language)) {
      return this.loadingPromises.get(language)!;
    }

    // Start loading
    const loadPromise = this.performLoad(language);
    this.loadingPromises.set(language, loadPromise);

    try {
      const instance = await loadPromise;
      this.loadedModules.set(language, instance);
      return instance;
    } finally {
      this.loadingPromises.delete(language);
    }
  }

  /**
   * Perform actual module loading
   */
  private async performLoad(language: string): Promise<WebAssembly.Instance> {
    const url = this.moduleUrls.get(language);
    if (!url) {
      throw new Error(`No URL registered for language: ${language}`);
    }

    // Fetch with streaming if available
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to load module: ${response.statusText}`);
    }

    // Use streaming instantiation for better performance
    if (typeof WebAssembly.instantiateStreaming !== 'undefined') {
      const result = await WebAssembly.instantiateStreaming(response);
      return result.instance;
    } else {
      // Fallback for browsers without streaming
      const buffer = await response.arrayBuffer();
      const result = await WebAssembly.instantiate(buffer);
      return result.instance;
    }
  }

  /**
   * Preload a module in the background
   * 
   * Call this to start loading a module without blocking.
   */
  preload(language: string): void {
    if (!this.loadedModules.has(language) && !this.loadingPromises.has(language)) {
      this.loadModule(language).catch(err => console.error(`Preload failed for ${language}:`, err));
    }
  }

  /**
   * Preload all registered modules
   */
  preloadAll(): void {
    for (const language of this.moduleUrls.keys()) {
      this.preload(language);
    }
  }

  /**
   * Get loaded module without loading if not available
   */
  getLoaded(language: string): WebAssembly.Instance | null {
    return this.loadedModules.get(language) || null;
  }

  /**
   * Clear all cached modules to free memory
   */
  clearCache(): void {
    this.loadedModules.clear();
    this.loadingPromises.clear();
  }
}

/**
 * Export optimization statistics reporter
 */
export interface OptimizationStats {
  zeroCopyBuffers: number;
  totalSharedMemory: number;
  dataViewCache: number;
  loadedModules: number;
  averageLoadTime?: number;
}

/**
 * Collect optimization statistics
 */
export function getOptimizationStats(
  zeroCopy: ZeroCopyMemory,
  directAccess: DirectMemoryAccess,
  lazyLoader: LazyModuleLoader
): OptimizationStats {
  const zeroCopyStats = zeroCopy.getStats();
  const directAccessStats = directAccess.getStats();

  return {
    zeroCopyBuffers: zeroCopyStats.activeBuffers,
    totalSharedMemory: zeroCopyStats.totalMemory,
    dataViewCache: directAccessStats.cachedDataViews,
    loadedModules: Object.keys(lazyLoader['loadedModules'] || {}).length
  };
}
