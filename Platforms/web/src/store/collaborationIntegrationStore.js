/**
 * Enhanced Collaboration Store with Backend Integration
 * Integrates WebSocket, REST API, and OT engine
 */

import { create } from 'zustand'
import { persist } from 'zustand/middleware'
import { collaborationAPI } from '../services/apiClient'
import { websocketClient } from '../services/websocketClient'
import { offlineSyncService } from '../services/offlineSyncService'
import OTEngine from '../services/otEngine'

export const useCollaborationIntegrationStore = create(
  persist(
    (set, get) => ({
      // Room state
      currentRoomId: null,
      currentRoom: null,
      roomMembers: [],

      // Connection state
      isConnected: false,
      isOnline: typeof navigator !== 'undefined' ? navigator.onLine : true,
      connectionId: null,

      // Collaborators
      collaborators: [],
      activeCursors: {},
      typingUsers: new Set(),

      // Editing state
      currentVersion: 0,
      documentContent: '',
      pendingOperations: [],
      appliedOperations: [],

      // OT Engine
      otEngine: new OTEngine(),

      // Sync state
      isSyncing: false,
      lastSyncTime: null,
      syncError: null,
      offlineQueueSize: 0,

      // Chat state
      messages: [],
      reactions: {},

      /**
       * Initialize collaboration for a room
       */
      initializeRoom: async (roomId, userId, username) => {
        try {
          set({ currentRoomId: roomId })

          // Get room details
          const roomResponse = await collaborationAPI.getRoom(roomId)
          set({ currentRoom: roomResponse })

          // Get room members
          const membersResponse = await collaborationAPI.getRoomMembers(roomId)
          set({ roomMembers: membersResponse })

          // Get initial sync data
          const syncResponse = await collaborationAPI.fullSync(roomId, 0)
          set({
            currentVersion: syncResponse.server_version,
            documentContent: syncResponse.snapshot?.content || '',
          })

          // Get initial messages
          const messagesResponse = await collaborationAPI.getRoomMessages(roomId, 50)
          set({ messages: messagesResponse })

          // Connect WebSocket for this room
          await websocketClient.connect()
          websocketClient.emit('join_room', {
            room_id: roomId,
            user_id: userId,
            username,
          })

          set({ isConnected: true })

          // Set up event listeners
          get().setupWebSocketListeners(roomId)
        } catch (error) {
          console.error('Failed to initialize room:', error)
          set({ syncError: error.message })
          throw error
        }
      },

      /**
       * Set up WebSocket event listeners
       */
      setupWebSocketListeners: (roomId) => {
        const { applyRemoteOperation, addCollaborator, removeCollaborator, updateCursor } = get()

        // Handle code changes
        websocketClient.on('code_change', (data) => {
          if (data.room_id === roomId) {
            applyRemoteOperation(data)
          }
        })

        // Handle cursor updates
        websocketClient.on('cursor_update', (data) => {
          if (data.room_id === roomId) {
            updateCursor(data.user_id, data.position)
          }
        })

        // Handle typing indicators
        websocketClient.on('typing', (data) => {
          if (data.room_id === roomId) {
            set((state) => {
              const updated = new Set(state.typingUsers)
              if (data.is_typing) {
                updated.add(data.user_id)
              } else {
                updated.delete(data.user_id)
              }
              return { typingUsers: updated }
            })
          }
        })

        // Handle new members
        websocketClient.on('user_joined', (data) => {
          if (data.room_id === roomId) {
            addCollaborator(data)
          }
        })

        // Handle member leaving
        websocketClient.on('user_left', (data) => {
          if (data.room_id === roomId) {
            removeCollaborator(data.user_id)
          }
        })

        // Handle chat messages
        websocketClient.on('chat_message', (data) => {
          if (data.room_id === roomId) {
            set((state) => ({
              messages: [...state.messages, data],
            }))
          }
        })

        // Handle presence updates
        websocketClient.on('presence_update', (data) => {
          if (data.room_id === roomId) {
            set((state) => ({
              collaborators: state.collaborators.map((c) =>
                c.id === data.user_id ? { ...c, status: data.status, lastSeen: data.timestamp } : c
              ),
            }))
          }
        })
      },

      /**
       * Apply local operation (client-initiated edit)
       */
      applyLocalOperation: async (operation) => {
        const { otEngine, currentRoomId, documentContent, isOnline } = get()

        try {
          // Apply to local content
          const newContent = otEngine.apply(operation, documentContent)
          set({ documentContent: newContent })

          // Add to pending operations
          set((state) => ({
            pendingOperations: [...state.pendingOperations, operation],
          }))

          if (isOnline && currentRoomId) {
            // Send to server via WebSocket
            websocketClient.emit('code_change', {
              room_id: currentRoomId,
              op_type: operation.type,
              position: operation.position,
              content: operation.content || '',
            })

            // Also persist via REST API
            try {
              await collaborationAPI.recordOperation(
                currentRoomId,
                operation.type,
                operation.position,
                operation.content || ''
              )
            } catch (error) {
              console.warn('Failed to persist operation:', error)
            }
          } else {
            // Queue for later sync
            const queuedOp = offlineSyncService.queueOperation(currentRoomId, operation)
            set((state) => ({
              offlineQueueSize: state.offlineQueueSize + 1,
            }))
          }

          set({ syncError: null })
        } catch (error) {
          console.error('Failed to apply local operation:', error)
          set({ syncError: error.message })
          throw error
        }
      },

      /**
       * Apply remote operation (server-initiated edit)
       */
      applyRemoteOperation: (data) => {
        const { otEngine, documentContent, currentVersion } = get()

        try {
          // Transform against pending operations if needed
          let operation = {
            type: data.type,
            position: data.position,
            content: data.content || '',
          }

          // Apply transformation if there are pending ops
          if (get().pendingOperations.length > 0) {
            operation = otEngine.transform(operation, get().pendingOperations)
          }

          // Apply to local content
          const newContent = otEngine.applyRemote(operation, documentContent)

          set({
            documentContent: newContent,
            currentVersion: Math.max(currentVersion, data.version || currentVersion + 1),
            appliedOperations: [...get().appliedOperations, data],
          })
        } catch (error) {
          console.error('Failed to apply remote operation:', error)
          set({ syncError: error.message })
        }
      },

      /**
       * Sync with server
       */
      sync: async () => {
        const { currentRoomId, currentVersion, isOnline } = get()

        if (!isOnline || !currentRoomId) return

        try {
          set({ isSyncing: true })

          const response = await collaborationAPI.fullSync(currentRoomId, currentVersion)

          set({
            currentVersion: response.server_version,
            isSyncing: false,
            lastSyncTime: Date.now(),
            syncError: null,
          })
        } catch (error) {
          console.error('Sync failed:', error)
          set({
            isSyncing: false,
            syncError: error.message,
          })
        }
      },

      /**
       * Add message to chat
       */
      sendMessage: async (content) => {
        const { currentRoomId } = get()
        if (!currentRoomId) return

        try {
          websocketClient.emit('chat_message', {
            room_id: currentRoomId,
            content,
          })
        } catch (error) {
          console.error('Failed to send message:', error)
          throw error
        }
      },

      /**
       * Add collaborator
       */
      addCollaborator: (collaborator) => {
        set((state) => ({
          collaborators: [...state.collaborators, collaborator],
        }))
      },

      /**
       * Remove collaborator
       */
      removeCollaborator: (userId) => {
        set((state) => ({
          collaborators: state.collaborators.filter((c) => c.id !== userId),
        }))
      },

      /**
       * Update cursor position
       */
      updateCursor: (userId, position) => {
        set((state) => ({
          activeCursors: {
            ...state.activeCursors,
            [userId]: position,
          },
        }))
      },

      /**
       * Update typing status
       */
      setTyping: (isTyping) => {
        const { currentRoomId } = get()
        if (currentRoomId) {
          websocketClient.emit('typing', {
            room_id: currentRoomId,
            is_typing: isTyping,
          })
        }
      },

      /**
       * Leave room
       */
      leaveRoom: async () => {
        const { currentRoomId } = get()

        if (currentRoomId) {
          try {
            websocketClient.emit('leave_room', {
              room_id: currentRoomId,
            })

            await collaborationAPI.leaveRoom(currentRoomId)
          } catch (error) {
            console.error('Error leaving room:', error)
          }
        }

        set({
          currentRoomId: null,
          currentRoom: null,
          roomMembers: [],
          collaborators: [],
          isConnected: false,
          documentContent: '',
          messages: [],
        })
      },

      /**
       * Handle online/offline status
       */
      setOnlineStatus: (isOnline) => {
        set({ isOnline })

        if (isOnline) {
          // Trigger sync when back online
          get().sync()
          // Sync offline queue
          offlineSyncService.syncQueue()
        }
      },
    }),
    {
      name: 'collaboration-integration-store',
      partialize: (state) => ({
        documentContent: state.documentContent,
        messages: state.messages,
        currentVersion: state.currentVersion,
        lastSyncTime: state.lastSyncTime,
      }),
    }
  )
)

// Listen for online/offline events
if (typeof window !== 'undefined') {
  window.addEventListener('online', () => {
    useCollaborationIntegrationStore.getState().setOnlineStatus(true)
  })

  window.addEventListener('offline', () => {
    useCollaborationIntegrationStore.getState().setOnlineStatus(false)
  })
}

export default useCollaborationIntegrationStore
