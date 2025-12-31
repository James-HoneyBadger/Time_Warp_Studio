/**
 * useWasmInterpreter Hook
 * React integration for WASM interpreter execution
 * Provides caching, error handling, and performance monitoring
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { WasmInterpreter, getWasmBridge, isWasmSupported } from '../src/wasm/wasm-loader';
import { logger } from '../src/utils/logger';
import type { ExecutionResult } from '../src/types';

interface WasmExecutionState {
  isExecuting: boolean;
  result: ExecutionResult | null;
  error: Error | null;
  duration: number;
}

interface WasmHookOptions {
  language: string;
  fallbackToServer?: boolean;
  enableCache?: boolean;
  enableProfiling?: boolean;
}

/**
 * Hook for executing code in WASM interpreters
 */
export function useWasmInterpreter(options: WasmHookOptions) {
  const {
    language,
    fallbackToServer = true,
    enableCache = true,
    enableProfiling = false
  } = options;

  const [state, setState] = useState<WasmExecutionState>({
    isExecuting: false,
    result: null,
    error: null,
    duration: 0
  });

  const bridgeRef = useRef(getWasmBridge(fallbackToServer));
  const cacheRef = useRef<Map<string, ExecutionResult>>(new Map());
  const interpreterRef = useRef<WasmInterpreter | null>(null);

  // Initialize interpreter on mount
  useEffect(() => {
    if (!isWasmSupported()) {
      setState(prev => ({
        ...prev,
        error: new Error('WebAssembly not supported in this browser')
      }));
      return;
    }

    const initInterpreter = async () => {
      try {
        interpreterRef.current = new WasmInterpreter();
        await interpreterRef.current.initialize(language);
        logger.info(`WASM interpreter initialized for ${language}`);
      } catch (error) {
        logger.warn(`Failed to initialize WASM interpreter for ${language}:`, error);
        interpreterRef.current = null;
      }
    };

    initInterpreter();

    return () => {
      if (interpreterRef.current) {
        interpreterRef.current.shutdown().catch(error => {
          logger.error(`Error shutting down interpreter:`, error);
        });
      }
    };
  }, [language]);

  /**
   * Execute code
   */
  const execute = useCallback(async (code: string): Promise<ExecutionResult> => {
    if (!code.trim()) {
      return {
        success: true,
        output: '',
        error: '',
        duration: 0
      };
    }

    // Check cache
    if (enableCache && cacheRef.current.has(code)) {
      return cacheRef.current.get(code)!;
    }

    setState(prev => ({ ...prev, isExecuting: true, error: null }));

    try {
      const startTime = performance.now();

      let result: ExecutionResult;

      // Try WASM first
      if (interpreterRef.current) {
        result = await interpreterRef.current.execute(code);
      } else if (fallbackToServer) {
        // Fallback to server (would call API here)
        result = {
          success: false,
          output: '',
          error: 'Server fallback not implemented in hook',
          duration: 0,
          isWasm: false
        };
      } else {
        throw new Error('No interpreter available');
      }

      const duration = performance.now() - startTime;
      result.duration = duration;

      // Cache result
      if (enableCache) {
        cacheRef.current.set(code, result);
      }

      // Profile if enabled
      if (enableProfiling) {
        logger.info(`${language} execution: ${duration.toFixed(2)}ms, isWasm: ${result.isWasm}`);
      }

      setState({
        isExecuting: false,
        result,
        error: null,
        duration
      });

      return result;

    } catch (error) {
      const err = error instanceof Error ? error : new Error(String(error));
      setState(prev => ({
        ...prev,
        isExecuting: false,
        error: err
      }));
      throw err;
    }
  }, [language, interpreterRef, fallbackToServer, enableCache, enableProfiling]);

  /**
   * Reset interpreter
   */
  const reset = useCallback(async () => {
    try {
      if (interpreterRef.current) {
        await interpreterRef.current.reset();
      }
      setState({
        isExecuting: false,
        result: null,
        error: null,
        duration: 0
      });
      cacheRef.current.clear();
    } catch (error) {
      logger.error('Error resetting interpreter:', error);
    }
  }, []);

  /**
   * Clear execution cache
   */
  const clearCache = useCallback(() => {
    cacheRef.current.clear();
  }, []);

  return {
    execute,
    reset,
    clearCache,
    ...state,
    isSupported: isWasmSupported()
  };
}

/**
 * Hook for managing multiple language interpreters
 */
export function useWasmBridge(fallbackToServer: boolean = true) {
  const [loadedLanguages, setLoadedLanguages] = useState<string[]>([]);
  const [error, setError] = useState<Error | null>(null);
  const bridgeRef = useRef(getWasmBridge(fallbackToServer));

  const execute = useCallback(
    async (language: string, code: string): Promise<ExecutionResult> => {
      try {
        return await bridgeRef.current.execute(language, code);
      } catch (err) {
        const error = err instanceof Error ? err : new Error(String(err));
        setError(error);
        throw error;
      }
    },
    []
  );

  const preload = useCallback(async (languages: string[]) => {
    try {
      await bridgeRef.current.preloadAll(languages);
      setLoadedLanguages(languages);
      setError(null);
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));
      setError(error);
    }
  }, []);

  const getStatus = useCallback(() => {
    return bridgeRef.current.getStats();
  }, []);

  useEffect(() => {
    return () => {
      bridgeRef.current.cleanup().catch(error => {
        logger.error('Error cleaning up WASM bridge:', error);
      });
    };
  }, []);

  return {
    execute,
    preload,
    getStatus,
    loadedLanguages,
    error,
    isSupported: isWasmSupported()
  };
}

/**
 * Hook for WASM performance monitoring
 */
export function useWasmPerformance() {
  const [metrics, setMetrics] = useState({
    totalExecutions: 0,
    totalDuration: 0,
    averageDuration: 0,
    minDuration: Infinity,
    maxDuration: 0,
    wasmCount: 0,
    serverCount: 0
  });

  const recordExecution = useCallback((duration: number, isWasm: boolean) => {
    setMetrics(prev => {
      const totalExecutions = prev.totalExecutions + 1;
      const totalDuration = prev.totalDuration + duration;

      return {
        totalExecutions,
        totalDuration,
        averageDuration: totalDuration / totalExecutions,
        minDuration: Math.min(prev.minDuration, duration),
        maxDuration: Math.max(prev.maxDuration, duration),
        wasmCount: prev.wasmCount + (isWasm ? 1 : 0),
        serverCount: prev.serverCount + (isWasm ? 0 : 1)
      };
    });
  }, []);

  const reset = useCallback(() => {
    setMetrics({
      totalExecutions: 0,
      totalDuration: 0,
      averageDuration: 0,
      minDuration: Infinity,
      maxDuration: 0,
      wasmCount: 0,
      serverCount: 0
    });
  }, []);

  return { metrics, recordExecution, reset };
}
