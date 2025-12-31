/**
 * Offline Sync Service
 * Handles queueing and syncing operations when offline
 */

class OfflineSyncService {
  constructor() {
    this.queue = []
    this.isOnline = navigator.onLine
    this.listeners = new Set()
    this.syncTimeout = null

    // Listen for online/offline events
    window.addEventListener('online', () => this.handleOnline())
    window.addEventListener('offline', () => this.handleOffline())
  }

  handleOnline() {
    this.isOnline = true
    this.notifyListeners('online')
    this.syncQueue()
  }

  handleOffline() {
    this.isOnline = false
    this.notifyListeners('offline')
  }

  /**
   * Add operation to queue when offline
   */
  queueOperation(roomId, operation) {
    const queuedOp = {
      ...operation,
      roomId,
      timestamp: Date.now(),
      id: `pending_${Date.now()}_${Math.random()}`,
      synced: false,
    }

    this.queue.push(queuedOp)
    this.persistQueue()
    this.notifyListeners('operation_queued', queuedOp)

    return queuedOp
  }

  /**
   * Get all pending operations
   */
  getPendingOperations(roomId = null) {
    if (roomId) {
      return this.queue.filter((op) => op.roomId === roomId && !op.synced)
    }
    return this.queue.filter((op) => !op.synced)
  }

  /**
   * Sync queued operations when back online
   */
  async syncQueue() {
    if (this.isOnline && this.queue.length > 0) {
      this.notifyListeners('sync_start')

      const pendingOps = this.getPendingOperations()
      const groupedByRoom = this.groupByRoom(pendingOps)

      try {
        for (const [roomId, ops] of Object.entries(groupedByRoom)) {
          for (const op of ops) {
            try {
              // This will be called via the collaboration store
              this.notifyListeners('sync_operation', { roomId, operation: op })
            } catch (error) {
              console.error(`Failed to sync operation ${op.id}:`, error)
              return // Stop on first error and retry later
            }
          }
        }

        // Clear queue after successful sync
        this.queue = this.queue.filter((op) => !op.synced)
        this.persistQueue()
        this.notifyListeners('sync_complete')
      } catch (error) {
        console.error('Queue sync failed:', error)
        this.notifyListeners('sync_error', error)
      }
    }
  }

  /**
   * Mark operation as synced
   */
  markSynced(operationId) {
    const op = this.queue.find((o) => o.id === operationId)
    if (op) {
      op.synced = true
      this.persistQueue()
    }
  }

  /**
   * Clear all pending operations for a room
   */
  clearRoom(roomId) {
    this.queue = this.queue.filter((op) => op.roomId !== roomId || op.synced)
    this.persistQueue()
  }

  /**
   * Clear entire queue
   */
  clearQueue() {
    this.queue = []
    this.persistQueue()
  }

  /**
   * Group operations by room
   */
  groupByRoom(operations) {
    return operations.reduce((acc, op) => {
      if (!acc[op.roomId]) {
        acc[op.roomId] = []
      }
      acc[op.roomId].push(op)
      return acc
    }, {})
  }

  /**
   * Persist queue to localStorage
   */
  persistQueue() {
    try {
      localStorage.setItem('offline_queue', JSON.stringify(this.queue))
    } catch (error) {
      console.warn('Failed to persist offline queue:', error)
    }
  }

  /**
   * Restore queue from localStorage
   */
  restoreQueue() {
    try {
      const stored = localStorage.getItem('offline_queue')
      if (stored) {
        this.queue = JSON.parse(stored)
      }
    } catch (error) {
      console.warn('Failed to restore offline queue:', error)
      this.queue = []
    }
  }

  /**
   * Subscribe to sync events
   */
  subscribe(listener) {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  /**
   * Notify all listeners
   */
  notifyListeners(event, data = null) {
    this.listeners.forEach((listener) => {
      try {
        listener(event, data)
      } catch (error) {
        console.error('Listener error:', error)
      }
    })
  }

  /**
   * Get queue stats
   */
  getStats() {
    return {
      total: this.queue.length,
      pending: this.getPendingOperations().length,
      synced: this.queue.filter((op) => op.synced).length,
      isOnline: this.isOnline,
    }
  }
}

export const offlineSyncService = new OfflineSyncService()
offlineSyncService.restoreQueue()

export default OfflineSyncService
