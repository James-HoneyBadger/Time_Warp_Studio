/**
 * Client-side Operational Transform (OT) Engine
 * Implements conflict-free collaborative editing
 */

class OTEngine {
  constructor() {
    this.version = 0
    this.operations = []
    this.undoStack = []
    this.redoStack = []
  }

  /**
   * Create an insert operation
   */
  insert(position, content) {
    return {
      type: 'insert',
      position,
      content,
      length: content.length,
    }
  }

  /**
   * Create a delete operation
   */
  delete(position, length) {
    return {
      type: 'delete',
      position,
      length,
    }
  }

  /**
   * Apply local operation and increment version
   */
  apply(operation, content) {
    try {
      let result = content

      if (operation.type === 'insert') {
        result = content.slice(0, operation.position) + operation.content + content.slice(operation.position)
      } else if (operation.type === 'delete') {
        result = content.slice(0, operation.position) + content.slice(operation.position + operation.length)
      }

      this.operations.push(operation)
      this.version++
      this.undoStack.push({ operation, content })
      this.redoStack = [] // Clear redo stack on new operation

      return result
    } catch (error) {
      console.error('OT apply error:', error)
      throw error
    }
  }

  /**
   * Transform operation against other operations
   * Used for resolving concurrent edits
   */
  transform(operation, againstOperations) {
    let transformed = { ...operation }

    for (const against of againstOperations) {
      transformed = this._transformSingle(transformed, against)
    }

    return transformed
  }

  /**
   * Transform single operation
   */
  _transformSingle(op, against) {
    const result = { ...op }

    if (op.type === 'insert' && against.type === 'insert') {
      if (against.position < op.position) {
        result.position += against.length
      } else if (against.position === op.position && against.content > op.content) {
        result.position += against.length
      }
    } else if (op.type === 'insert' && against.type === 'delete') {
      if (against.position < op.position) {
        result.position -= Math.min(against.length, op.position - against.position)
      } else if (against.position < op.position + op.length) {
        result.position -= Math.min(against.length, op.position + op.length - against.position)
      }
    } else if (op.type === 'delete' && against.type === 'insert') {
      if (against.position < op.position) {
        result.position += against.length
      }
      if (against.position < op.position) {
        result.length += against.length
      }
    } else if (op.type === 'delete' && against.type === 'delete') {
      if (against.position < op.position) {
        result.position -= against.length
      } else if (against.position < op.position + op.length) {
        result.length -= Math.min(against.length, op.position + op.length - against.position)
      }
    }

    return result
  }

  /**
   * Apply remote operation to local content
   */
  applyRemote(operation, content) {
    return this.apply(operation, content)
  }

  /**
   * Resolve conflict between local and remote operations
   */
  resolveConflict(localOp, remoteOp) {
    // Operational Transform: transform local op against remote op
    const transformedLocal = this.transform(localOp, [remoteOp])
    const transformedRemote = this.transform(remoteOp, [localOp])

    return {
      localOp: transformedLocal,
      remoteOp: transformedRemote,
    }
  }

  /**
   * Undo last operation
   */
  undo() {
    if (this.undoStack.length === 0) return null

    const { operation, content } = this.undoStack.pop()
    this.redoStack.push(operation)
    this.version = Math.max(0, this.version - 1)

    return {
      operation: this._inverse(operation),
      content,
    }
  }

  /**
   * Redo last undone operation
   */
  redo() {
    if (this.redoStack.length === 0) return null

    const operation = this.redoStack.pop()
    this.undoStack.push({ operation, content: null })
    this.version++

    return operation
  }

  /**
   * Get inverse of operation
   */
  _inverse(operation) {
    if (operation.type === 'insert') {
      return this.delete(operation.position, operation.length)
    } else if (operation.type === 'delete') {
      return {
        type: 'insert',
        position: operation.position,
        content: '', // Would need original content
        length: operation.length,
      }
    }
    return operation
  }

  /**
   * Compose multiple operations into one
   */
  compose(operations) {
    if (operations.length === 0) return null
    if (operations.length === 1) return operations[0]

    let composed = { ...operations[0] }
    for (let i = 1; i < operations.length; i++) {
      composed = this._composePair(composed, operations[i])
    }
    return composed
  }

  /**
   * Compose two operations
   */
  _composePair(op1, op2) {
    // This is simplified; full implementation would handle all cases
    if (op1.type === 'insert' && op2.type === 'insert') {
      if (op1.position + op1.length === op2.position) {
        return {
          type: 'insert',
          position: op1.position,
          content: op1.content + op2.content,
          length: op1.length + op2.length,
        }
      }
    }
    return op2 // Return second operation if can't compose
  }

  /**
   * Get operation history
   */
  getHistory() {
    return this.operations
  }

  /**
   * Get current version
   */
  getVersion() {
    return this.version
  }

  /**
   * Reset engine state
   */
  reset() {
    this.version = 0
    this.operations = []
    this.undoStack = []
    this.redoStack = []
  }
}

export default OTEngine
