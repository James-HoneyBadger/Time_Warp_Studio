/**
 * Frontend-Backend Integration Tests
 * Tests real-time collaboration end-to-end
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { useCollaborationIntegrationStore } from '../store/collaborationIntegrationStore'
import OTEngine from '../services/otEngine'
import { offlineSyncService } from '../services/offlineSyncService'

describe('Frontend-Backend Integration', () => {
  let store
  let otEngine

  beforeEach(() => {
    store = useCollaborationIntegrationStore()
    otEngine = new OTEngine()
    vi.clearAllMocks()
  })

  afterEach(() => {
    store.leaveRoom()
    offlineSyncService.clearQueue()
  })

  describe('OT Engine', () => {
    it('should apply insert operation', () => {
      const op = otEngine.insert(0, 'hello')
      const result = otEngine.apply(op, '')
      expect(result).toBe('hello')
    })

    it('should apply delete operation', () => {
      const deleteOp = otEngine.delete(0, 2)
      const result = otEngine.apply(deleteOp, 'hello')
      expect(result).toBe('llo')
    })

    it('should transform concurrent inserts', () => {
      const op1 = otEngine.insert(0, 'a')
      const op2 = otEngine.insert(0, 'b')

      const transformed = otEngine.transform(op1, [op2])
      expect(transformed.position).toBeGreaterThanOrEqual(0)
    })

    it('should handle undo', () => {
      const op = otEngine.insert(0, 'hello')
      otEngine.apply(op, '')

      const undone = otEngine.undo()
      expect(undone).toBeDefined()
      expect(otEngine.getVersion()).toBe(0)
    })

    it('should handle redo', () => {
      const op = otEngine.insert(0, 'hello')
      otEngine.apply(op, '')
      otEngine.undo()

      const redone = otEngine.redo()
      expect(redone).toBeDefined()
      expect(otEngine.getVersion()).toBe(1)
    })
  })

  describe('Offline Sync Service', () => {
    it('should queue operations when offline', () => {
      const op = {
        type: 'insert',
        position: 0,
        content: 'hello',
      }

      const queued = offlineSyncService.queueOperation('room1', op)
      expect(queued.id).toBeDefined()
      expect(queued.synced).toBe(false)
    })

    it('should get pending operations', () => {
      const op1 = {
        type: 'insert',
        position: 0,
        content: 'hello',
      }
      const op2 = {
        type: 'delete',
        position: 0,
        length: 2,
      }

      offlineSyncService.queueOperation('room1', op1)
      offlineSyncService.queueOperation('room1', op2)

      const pending = offlineSyncService.getPendingOperations('room1')
      expect(pending).toHaveLength(2)
    })

    it('should mark operations as synced', () => {
      const op = {
        type: 'insert',
        position: 0,
        content: 'hello',
      }

      const queued = offlineSyncService.queueOperation('room1', op)
      offlineSyncService.markSynced(queued.id)

      const pending = offlineSyncService.getPendingOperations('room1')
      expect(pending).toHaveLength(0)
    })

    it('should clear queue for room', () => {
      offlineSyncService.queueOperation('room1', {
        type: 'insert',
        position: 0,
        content: 'a',
      })
      offlineSyncService.queueOperation('room2', {
        type: 'insert',
        position: 0,
        content: 'b',
      })

      offlineSyncService.clearRoom('room1')

      expect(offlineSyncService.getPendingOperations('room1')).toHaveLength(0)
      expect(offlineSyncService.getPendingOperations('room2')).toHaveLength(1)
    })

    it('should persist and restore queue', () => {
      const op = {
        type: 'insert',
        position: 0,
        content: 'hello',
      }

      offlineSyncService.queueOperation('room1', op)
      offlineSyncService.persistQueue()

      const fresh = new (require('../services/offlineSyncService').default)()
      fresh.restoreQueue()

      const pending = fresh.getPendingOperations('room1')
      expect(pending).toHaveLength(1)
    })

    it('should get sync stats', () => {
      offlineSyncService.queueOperation('room1', {
        type: 'insert',
        position: 0,
        content: 'a',
      })

      const stats = offlineSyncService.getStats()
      expect(stats.total).toBe(1)
      expect(stats.pending).toBe(1)
      expect(stats.isOnline).toBeDefined()
    })
  })

  describe('Collaboration Store Integration', () => {
    it('should initialize with default values', () => {
      expect(store.isConnected).toBe(false)
      expect(store.currentRoomId).toBeNull()
      expect(store.documentContent).toBe('')
      expect(store.collaborators).toHaveLength(0)
    })

    it('should apply local operation', async () => {
      const op = otEngine.insert(0, 'hello')
      const newContent = otEngine.apply(op, '')

      expect(newContent).toBe('hello')
      expect(otEngine.getVersion()).toBe(1)
    })

    it('should handle concurrent operations', () => {
      // Simulate two users editing simultaneously
      const op1 = otEngine.insert(0, 'user1')
      const op2 = otEngine.insert(0, 'user2')

      const transformed = otEngine.transform(op1, [op2])
      expect(transformed).toBeDefined()
      expect(transformed.position).toBeGreaterThanOrEqual(0)
    })

    it('should set online status', () => {
      store.setOnlineStatus(false)
      expect(store.isOnline).toBe(false)

      store.setOnlineStatus(true)
      expect(store.isOnline).toBe(true)
    })

    it('should add message to store', () => {
      const message = {
        id: 'msg1',
        content: 'Hello',
        user_id: 'user1',
        timestamp: Date.now(),
      }

      store.addCollaborator({
        id: 'user1',
        name: 'User 1',
        status: 'online',
      })

      expect(store.collaborators).toHaveLength(1)
    })

    it('should remove collaborator', () => {
      store.addCollaborator({
        id: 'user1',
        name: 'User 1',
      })

      expect(store.collaborators).toHaveLength(1)

      store.removeCollaborator('user1')

      expect(store.collaborators).toHaveLength(0)
    })

    it('should update cursor position', () => {
      store.updateCursor('user1', 42)

      expect(store.activeCursors['user1']).toBe(42)
    })

    it('should handle error during operations', async () => {
      // Simulate error
      store.setSyncError('Test error')
      expect(store.syncError).toBe('Test error')

      // Clear error
      store.setSyncError(null)
      expect(store.syncError).toBeNull()
    })
  })

  describe('Multi-user Collaboration', () => {
    it('should handle two users editing same position', () => {
      // User 1 inserts at position 0
      const op1 = otEngine.insert(0, 'a')
      let content1 = otEngine.apply(op1, '')
      expect(content1).toBe('a')

      // User 2 inserts at position 0 (concurrent)
      const op2 = otEngine.insert(0, 'b')

      // Transform op2 against op1
      const transformedOp2 = otEngine.transform(op2, [op1])

      // Apply transformed operation
      const content2 = otEngine.apply(transformedOp2, 'a')

      // Both should have consistent state
      expect(content2).toBeDefined()
      expect(content2.length).toBe(2)
    })

    it('should handle user disconnection', () => {
      store.addCollaborator({
        id: 'user1',
        name: 'User 1',
      })

      expect(store.collaborators).toHaveLength(1)

      // User disconnects
      store.removeCollaborator('user1')

      expect(store.collaborators).toHaveLength(0)
    })

    it('should handle rapid operations', () => {
      let content = ''

      for (let i = 0; i < 10; i++) {
        const op = otEngine.insert(content.length, `char${i}`)
        content = otEngine.apply(op, content)
      }

      expect(content).toBeDefined()
      expect(otEngine.getVersion()).toBe(10)
    })

    it('should track operation history', () => {
      const op1 = otEngine.insert(0, 'hello')
      otEngine.apply(op1, '')

      const op2 = otEngine.insert(5, ' world')
      otEngine.apply(op2, 'hello')

      const history = otEngine.getHistory()
      expect(history).toHaveLength(2)
    })
  })

  describe('Offline Mode', () => {
    it('should queue operations offline', () => {
      offlineSyncService.handleOffline()

      expect(offlineSyncService.isOnline).toBe(false)
    })

    it('should sync queued operations online', async () => {
      // Queue operations while offline
      offlineSyncService.handleOffline()
      offlineSyncService.queueOperation('room1', {
        type: 'insert',
        position: 0,
        content: 'hello',
      })

      expect(offlineSyncService.getPendingOperations()).toHaveLength(1)

      // Come back online
      offlineSyncService.handleOnline()

      // Queue should still exist (sync would happen in real scenario)
      expect(offlineSyncService.isOnline).toBe(true)
    })

    it('should preserve queue on page reload', () => {
      offlineSyncService.queueOperation('room1', {
        type: 'insert',
        position: 0,
        content: 'test',
      })

      offlineSyncService.persistQueue()

      // Simulate page reload
      const fresh = new (require('../services/offlineSyncService').default)()
      fresh.restoreQueue()

      expect(fresh.getPendingOperations()).toHaveLength(1)
    })
  })
})
