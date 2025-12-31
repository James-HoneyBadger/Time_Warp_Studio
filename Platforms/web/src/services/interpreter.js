import { executionAPI } from './apiClient'

let wasmModule = null

// Initialize WASM module
export async function initializeWasm() {
  try {
    // This will be populated when Rust module is compiled
    // For now, return dummy function
    return {
      execute: (code, language) => `✅ Execution complete\n`,
    }
  } catch (error) {
    console.error('Failed to initialize WASM module:', error)
    throw error
  }
}

// Execute code
export async function executeCode(code, language, projectId) {
  try {
    // Attempt server-side execution first
    const response = await executionAPI.run(projectId, code, language)
    return response.data.output || ''
  } catch (error) {
    // Fallback to WASM if available
    if (wasmModule) {
      return wasmModule.execute(code, language)
    }
    throw new Error('Execution failed: ' + error.message)
  }
}

// Cancel execution
export async function cancelExecution(executionId) {
  try {
    const response = await executionAPI.cancel(executionId)
    return response.data
  } catch (error) {
    throw new Error('Failed to cancel execution: ' + error.message)
  }
}

// Get execution history
export async function getExecutionHistory(projectId) {
  try {
    const response = await executionAPI.getHistory(projectId)
    return response.data
  } catch (error) {
    throw new Error('Failed to fetch history: ' + error.message)
  }
}

export default {
  initializeWasm,
  executeCode,
  cancelExecution,
  getExecutionHistory,
}
