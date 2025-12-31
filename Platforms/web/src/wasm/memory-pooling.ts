/**
 * Memory Pooling Strategy Module
 * 
 * Implements object pooling patterns to reduce GC pressure and allocation overhead.
 * Reuses buffer and object instances rather than creating new ones each time.
 * 
 * Target: 20% memory reduction through pooling and reuse
 */

/**
 * Pool configuration for different memory patterns
 */
export interface PoolConfig {
  minSize: number;      // Minimum objects to pre-allocate
  maxSize: number;      // Maximum objects to pool
  growthFactor: number; // How much to grow pool when empty
  resetOnReturn?: boolean; // Whether to reset object state on return
}

/**
 * Object pool for reusable buffers
 * 
 * Implements a FIFO pool to reduce allocation overhead for frequently
 * created and destroyed buffers.
 */
export class BufferPool {
  private available: Uint8Array[] = [];
  private inUse: Set<Uint8Array> = new Set();
  private poolSize: number;
  private bufferSize: number;
  private config: PoolConfig;
  private stats = {
    allocations: 0,
    reallocations: 0,
    poolHits: 0,
    poolMisses: 0
  };

  constructor(bufferSize: number, config: PoolConfig) {
    this.bufferSize = bufferSize;
    this.config = config;
    this.poolSize = 0;

    // Pre-allocate minimum pool size
    for (let i = 0; i < config.minSize; i++) {
      this.available.push(new Uint8Array(bufferSize));
      this.poolSize++;
    }
    this.stats.allocations = config.minSize;
  }

  /**
   * Acquire a buffer from pool (or allocate if none available)
   */
  acquire(): Uint8Array {
    let buffer: Uint8Array;

    if (this.available.length > 0) {
      buffer = this.available.pop()!;
      this.stats.poolHits++;
    } else if (this.poolSize < this.config.maxSize) {
      // Allocate new buffer if under limit
      buffer = new Uint8Array(this.bufferSize);
      this.poolSize++;
      this.stats.allocations++;
      this.stats.poolMisses++;
    } else {
      // Pool is full, must allocate temporarily (will not be returned to pool)
      buffer = new Uint8Array(this.bufferSize);
      this.stats.reallocations++;
      this.stats.poolMisses++;
    }

    this.inUse.add(buffer);
    return buffer;
  }

  /**
   * Return a buffer to the pool for reuse
   */
  release(buffer: Uint8Array): void {
    if (!this.inUse.has(buffer)) {
      throw new Error('Buffer not from this pool');
    }

    this.inUse.delete(buffer);

    // Only return to pool if we have space
    if (this.available.length < this.config.maxSize) {
      // Optionally reset buffer state
      if (this.config.resetOnReturn) {
        buffer.fill(0);
      }
      this.available.push(buffer);
    }
  }

  /**
   * Release all buffers and reset pool
   */
  clear(): void {
    this.available.clear();
    this.inUse.clear();
    this.poolSize = 0;
    this.stats.allocations = 0;
    this.stats.reallocations = 0;
  }

  /**
   * Get pool statistics
   */
  getStats() {
    return {
      bufferSize: this.bufferSize,
      availableCount: this.available.length,
      inUseCount: this.inUse.size,
      totalPooled: this.poolSize,
      maxPoolSize: this.config.maxSize,
      ...this.stats
    };
  }

  /**
   * Get memory usage of pool
   */
  getMemoryUsage(): number {
    return this.poolSize * this.bufferSize;
  }
}

/**
 * Multi-size buffer pool manager
 * 
 * Maintains separate pools for different buffer sizes to match allocation patterns.
 * Common sizes: 64B, 256B, 1KB, 4KB, 16KB, 64KB
 */
export class MultiSizeBufferPool {
  private pools: Map<number, BufferPool> = new Map();
  private defaultConfig: PoolConfig;
  private sizeMap: number[] = [];

  constructor(defaultConfig?: PoolConfig) {
    this.defaultConfig = defaultConfig || {
      minSize: 10,
      maxSize: 100,
      growthFactor: 2,
      resetOnReturn: true
    };
  }

  /**
   * Register a buffer size with its pool configuration
   */
  registerSize(size: number, config?: PoolConfig): void {
    const poolConfig = config || this.defaultConfig;
    this.pools.set(size, new BufferPool(size, poolConfig));
    this.sizeMap.push(size);
    this.sizeMap.sort((a, b) => a - b);
  }

  /**
   * Get or create pool for a size
   */
  private getPool(size: number): BufferPool {
    // Try exact match first
    if (this.pools.has(size)) {
      return this.pools.get(size)!;
    }

    // Find next larger registered size
    const nextSize = this.sizeMap.find(s => s >= size);
    if (nextSize) {
      return this.pools.get(nextSize)!;
    }

    // Create new pool for this size
    const pool = new BufferPool(size, this.defaultConfig);
    this.pools.set(size, pool);
    this.sizeMap.push(size);
    this.sizeMap.sort((a, b) => a - b);
    return pool;
  }

  /**
   * Acquire a buffer of at least the specified size
   */
  acquire(size: number): Uint8Array {
    const pool = this.getPool(size);
    return pool.acquire();
  }

  /**
   * Release a buffer back to its pool
   */
  release(buffer: Uint8Array): void {
    const pool = this.getPool(buffer.length);
    pool.release(buffer);
  }

  /**
   * Get statistics for all pools
   */
  getAllStats() {
    const stats: Record<string, any> = {};
    for (const [size, pool] of this.pools) {
      stats[`${size}B`] = pool.getStats();
    }
    return stats;
  }

  /**
   * Get total memory usage across all pools
   */
  getTotalMemoryUsage(): number {
    let total = 0;
    for (const pool of this.pools.values()) {
      total += pool.getMemoryUsage();
    }
    return total;
  }

  /**
   * Clear all pools
   */
  clear(): void {
    for (const pool of this.pools.values()) {
      pool.clear();
    }
    this.pools.clear();
    this.sizeMap = [];
  }
}

/**
 * Object pool for generic objects (reduce GC pressure)
 * 
 * Used for frequently created/destroyed objects like result wrappers.
 */
export class ObjectPool<T> {
  private available: T[] = [];
  private inUse: Set<T> = new Set();
  private factory: () => T;
  private reset: (obj: T) => void;
  private config: PoolConfig;
  private stats = {
    allocations: 0,
    poolHits: 0,
    poolMisses: 0
  };

  constructor(
    factory: () => T,
    reset: (obj: T) => void,
    config: PoolConfig
  ) {
    this.factory = factory;
    this.reset = reset;
    this.config = config;

    // Pre-allocate
    for (let i = 0; i < config.minSize; i++) {
      this.available.push(factory());
      this.stats.allocations++;
    }
  }

  /**
   * Acquire object from pool
   */
  acquire(): T {
    let obj: T;

    if (this.available.length > 0) {
      obj = this.available.pop()!;
      this.stats.poolHits++;
    } else {
      obj = this.factory();
      this.stats.allocations++;
      this.stats.poolMisses++;
    }

    this.inUse.add(obj);
    return obj;
  }

  /**
   * Return object to pool
   */
  release(obj: T): void {
    if (!this.inUse.has(obj)) {
      throw new Error('Object not from this pool');
    }

    this.inUse.delete(obj);

    // Reset object state
    this.reset(obj);

    // Return to pool if space available
    if (this.available.length < this.config.maxSize) {
      this.available.push(obj);
    }
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      availableCount: this.available.length,
      inUseCount: this.inUse.size,
      maxPoolSize: this.config.maxSize,
      ...this.stats
    };
  }

  /**
   * Clear pool
   */
  clear(): void {
    this.available = [];
    this.inUse.clear();
  }
}

/**
 * String interning pool to reduce duplicate string allocations
 * 
 * Caches frequently used strings (error messages, keywords, etc.)
 * to reduce string allocation overhead.
 */
export class StringInternPool {
  private interned: Map<string, string> = new Map();
  private stats = {
    hits: 0,
    misses: 0,
    totalInterned: 0
  };

  /**
   * Intern a string (get canonical reference)
   */
  intern(str: string): string {
    if (this.interned.has(str)) {
      this.stats.hits++;
      return this.interned.get(str)!;
    }

    this.interned.set(str, str);
    this.stats.misses++;
    this.stats.totalInterned++;
    return str;
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      ...this.stats,
      hitRate: this.stats.hits / (this.stats.hits + this.stats.misses) * 100
    };
  }

  /**
   * Clear pool
   */
  clear(): void {
    this.interned.clear();
    this.stats = { hits: 0, misses: 0, totalInterned: 0 };
  }
}

/**
 * Result wrapper pool for execution results
 * 
 * Reduces allocation overhead for frequently created result objects.
 */
export interface ExecutionResult {
  success: boolean;
  output: string;
  error?: string;
  graphics?: any[];
  duration: number;
  memoryUsed: number;
  reset(): void;
}

/**
 * Factory for result objects
 */
export class ExecutionResultFactory {
  static create(): ExecutionResult {
    return {
      success: false,
      output: '',
      error: undefined,
      graphics: undefined,
      duration: 0,
      memoryUsed: 0,
      reset() {
        this.success = false;
        this.output = '';
        this.error = undefined;
        this.graphics = undefined;
        this.duration = 0;
        this.memoryUsed = 0;
      }
    };
  }
}

/**
 * Array pool for frequently resized arrays
 * 
 * Useful for graphics command arrays, trace arrays, etc.
 */
export class ArrayPool<T> {
  private pools: Map<number, T[][]> = new Map();
  private inUse: Set<T[]> = new Set();

  /**
   * Acquire array of at least specified capacity
   */
  acquire(minCapacity: number): T[] {
    let arr: T[];

    if (!this.pools.has(minCapacity)) {
      this.pools.set(minCapacity, []);
    }

    const pool = this.pools.get(minCapacity)!;
    if (pool.length > 0) {
      arr = pool.pop()!;
      arr.length = 0; // Clear without deallocating
    } else {
      arr = new Array(minCapacity);
      arr.length = 0;
    }

    this.inUse.add(arr);
    return arr;
  }

  /**
   * Release array back to pool
   */
  release(arr: T[]): void {
    if (!this.inUse.has(arr)) {
      throw new Error('Array not from this pool');
    }

    this.inUse.delete(arr);
    const capacity = arr.length > 0 ? arr.length * 2 : 16;

    if (!this.pools.has(capacity)) {
      this.pools.set(capacity, []);
    }

    arr.length = 0;
    this.pools.get(capacity)!.push(arr);
  }

  /**
   * Get statistics
   */
  getStats() {
    const stats: Record<string, any> = {};
    for (const [capacity, pool] of this.pools) {
      stats[`${capacity}`] = pool.length;
    }
    return stats;
  }

  /**
   * Clear all pools
   */
  clear(): void {
    this.pools.clear();
    this.inUse.clear();
  }
}

/**
 * Combined pooling manager for all object types
 */
export class PoolingManager {
  private bufferPool: MultiSizeBufferPool;
  private resultPool: ObjectPool<ExecutionResult>;
  private stringPool: StringInternPool;
  private commandArrayPool: ArrayPool<any>;

  constructor() {
    this.bufferPool = new MultiSizeBufferPool({
      minSize: 5,
      maxSize: 50,
      growthFactor: 2,
      resetOnReturn: true
    });

    // Register common buffer sizes
    this.bufferPool.registerSize(64, { minSize: 10, maxSize: 100, growthFactor: 2 });
    this.bufferPool.registerSize(256, { minSize: 10, maxSize: 100, growthFactor: 2 });
    this.bufferPool.registerSize(1024, { minSize: 10, maxSize: 50, growthFactor: 2 });
    this.bufferPool.registerSize(4096, { minSize: 5, maxSize: 30, growthFactor: 2 });
    this.bufferPool.registerSize(16384, { minSize: 3, maxSize: 20, growthFactor: 2 });
    this.bufferPool.registerSize(65536, { minSize: 2, maxSize: 10, growthFactor: 2 });

    this.resultPool = new ObjectPool<ExecutionResult>(
      ExecutionResultFactory.create,
      (obj) => obj.reset(),
      { minSize: 10, maxSize: 100, growthFactor: 2 }
    );

    this.stringPool = new StringInternPool();
    this.commandArrayPool = new ArrayPool<any>();
  }

  /**
   * Acquire buffer
   */
  acquireBuffer(size: number): Uint8Array {
    return this.bufferPool.acquire(size);
  }

  /**
   * Release buffer
   */
  releaseBuffer(buffer: Uint8Array): void {
    this.bufferPool.release(buffer);
  }

  /**
   * Acquire result wrapper
   */
  acquireResult(): ExecutionResult {
    return this.resultPool.acquire();
  }

  /**
   * Release result wrapper
   */
  releaseResult(result: ExecutionResult): void {
    this.resultPool.release(result);
  }

  /**
   * Intern string
   */
  internString(str: string): string {
    return this.stringPool.intern(str);
  }

  /**
   * Acquire command array
   */
  acquireCommandArray(capacity: number): any[] {
    return this.commandArrayPool.acquire(capacity);
  }

  /**
   * Release command array
   */
  releaseCommandArray(arr: any[]): void {
    this.commandArrayPool.release(arr);
  }

  /**
   * Get all statistics
   */
  getAllStats() {
    return {
      buffers: this.bufferPool.getAllStats(),
      results: this.resultPool.getStats(),
      strings: this.stringPool.getStats(),
      commandArrays: this.commandArrayPool.getStats()
    };
  }

  /**
   * Get total memory usage
   */
  getTotalMemoryUsage(): number {
    return this.bufferPool.getTotalMemoryUsage();
  }

  /**
   * Clear all pools
   */
  clear(): void {
    this.bufferPool.clear();
    this.resultPool.clear();
    this.stringPool.clear();
    this.commandArrayPool.clear();
  }
}

// Export singleton instance
export const poolingManager = new PoolingManager();
