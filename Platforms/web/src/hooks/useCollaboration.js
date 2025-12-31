/**
 * React Hooks for Collaboration Features
 * Provides hooks for components to use collaboration functionality
 */

import { useEffect, useCallback, useRef } from 'react'
import { useCollaborationIntegrationStore } from '../store/collaborationIntegrationStore'
import { websocketClient } from '../services/websocketClient'
import { offlineSyncService } from '../services/offlineSyncService'
import OTEngine from '../services/otEngine'

/**
 * Hook for managing collaborative editing
 */
export const useCollaborativeEditor = (roomId, userId, username) => {
  const store = useCollaborationIntegrationStore()
  const otEngineRef = useRef(new OTEngine())
  const syncTimeoutRef = useRef(null)

  // Initialize room on mount
  useEffect(() => {
    if (roomId && userId && username) {
      store.initializeRoom(roomId, userId, username).catch(console.error)
    }

    // Cleanup on unmount
    return () => {
      store.leaveRoom()
    }
  }, [roomId, userId, username])

  // Sync periodically
  useEffect(() => {
    syncTimeoutRef.current = setInterval(() => {
      if (store.isOnline && roomId) {
        store.sync()
      }
    }, 5000) // Sync every 5 seconds

    return () => clearInterval(syncTimeoutRef.current)
  }, [store.isOnline, roomId])

  // Listen for offline queue sync
  useEffect(() => {
    const unsubscribe = offlineSyncService.subscribe((event, data) => {
      if (event === 'sync_operation' && data.roomId === roomId) {
        // Handle offline operation sync
        store.applyLocalOperation(data.operation).catch(console.error)
      }
    })

    return () => unsubscribe()
  }, [roomId])

  const handleInsert = useCallback(
    (position, content) => {
      const op = otEngineRef.current.insert(position, content)
      store.applyLocalOperation(op).catch(console.error)
    },
    [store]
  )

  const handleDelete = useCallback(
    (position, length) => {
      const op = otEngineRef.current.delete(position, length)
      store.applyLocalOperation(op).catch(console.error)
    },
    [store]
  )

  const handleUndo = useCallback(() => {
    const result = otEngineRef.current.undo()
    if (result) {
      store.applyLocalOperation(result.operation).catch(console.error)
    }
  }, [store])

  const handleRedo = useCallback(() => {
    const result = otEngineRef.current.redo()
    if (result) {
      store.applyLocalOperation(result).catch(console.error)
    }
  }, [store])

  return {
    ...store,
    handleInsert,
    handleDelete,
    handleUndo,
    handleRedo,
  }
}

/**
 * Hook for managing collaborators
 */
export const useCollaborators = () => {
  const store = useCollaborationIntegrationStore()

  const handleRemoteUserJoined = useCallback((user) => {
    store.addCollaborator(user)
  }, [store])

  const handleRemoteUserLeft = useCallback((userId) => {
    store.removeCollaborator(userId)
  }, [store])

  const handleRemoteUserCursor = useCallback((userId, position) => {
    store.updateCursor(userId, position)
  }, [store])

  return {
    collaborators: store.collaborators,
    activeCursors: store.activeCursors,
    typingUsers: store.typingUsers,
    handleRemoteUserJoined,
    handleRemoteUserLeft,
    handleRemoteUserCursor,
  }
}

/**
 * Hook for managing chat
 */
export const useCollaborativeChat = () => {
  const store = useCollaborationIntegrationStore()

  const sendMessage = useCallback(
    (content) => {
      store.sendMessage(content).catch(console.error)
    },
    [store]
  )

  const setTyping = useCallback(
    (isTyping) => {
      store.setTyping(isTyping)
    },
    [store]
  )

  return {
    messages: store.messages,
    typingUsers: store.typingUsers,
    sendMessage,
    setTyping,
  }
}

/**
 * Hook for managing sync status
 */
export const useSyncStatus = () => {
  const store = useCollaborationIntegrationStore()

  return {
    isOnline: store.isOnline,
    isSyncing: store.isSyncing,
    syncError: store.syncError,
    lastSyncTime: store.lastSyncTime,
    offlineQueueSize: store.offlineQueueSize,
    sync: store.sync,
  }
}

/**
 * Hook for managing presence
 */
export const usePresence = (userId, username) => {
  const store = useCollaborationIntegrationStore()

  useEffect(() => {
    // Update presence on mount
    websocketClient.emit('presence_update', {
      user_id: userId,
      status: 'online',
      username,
    })

    // Cleanup on unmount
    return () => {
      websocketClient.emit('presence_update', {
        user_id: userId,
        status: 'offline',
      })
    }
  }, [userId, username])

  const updateStatus = useCallback((status) => {
    websocketClient.emit('presence_update', {
      user_id: userId,
      status,
    })
  }, [userId])

  return {
    collaborators: store.collaborators,
    updateStatus,
  }
}

/**
 * Hook for managing room
 */
export const useCollaborativeRoom = (roomId) => {
  const store = useCollaborationIntegrationStore()

  const getMembers = useCallback(async () => {
    return store.roomMembers
  }, [store.roomMembers])

  const getMessages = useCallback(async (limit = 50, offset = 0) => {
    return store.messages
  }, [store.messages])

  const leaveRoom = useCallback(async () => {
    await store.leaveRoom()
  }, [store])

  return {
    room: store.currentRoom,
    members: store.roomMembers,
    messages: store.messages,
    getMembers,
    getMessages,
    leaveRoom,
  }
}

export default {
  useCollaborativeEditor,
  useCollaborators,
  useCollaborativeChat,
  useSyncStatus,
  usePresence,
  useCollaborativeRoom,
}
