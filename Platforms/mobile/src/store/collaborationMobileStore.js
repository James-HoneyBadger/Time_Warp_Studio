/**
 * React Native Collaboration Store
 * Mobile-optimized Zustand store with battery and network awareness
 */

import { create } from 'zustand'
import { persist } from 'zustand/middleware'
import AsyncStorage from '@react-native-async-storage/async-storage'
import { OTEngine } from '../services/otEngine'

const createCollaborationMobileStore = () =>
  create(
    persist(
      (set, get) => ({
        // Core state
        roomId: null,
        userId: null,
        username: null,
        document: '',
        collaborators: [],
        messages: [],
        operations: [],
        cursorPositions: {},
        typingUsers: [],

        // Sync state
        isOnline: true,
        isSyncing: false,
        lastSyncTime: null,
        syncError: null,
        offlineQueue: [],
        offlineQueueSize: 0,

        // UI state
        cursorPosition: 0,
        selectionStart: 0,
        selectionEnd: 0,
        isEditing: false,

        // Performance state
        isBatteryLow: false,
        networkLatency: 0,
        lastPingTime: Date.now(),

        // Room state
        roomDetails: null,
        isRoomOwner: false,

        // OT engine
        otEngine: null,
        documentVersion: 0,
        ackedVersion: 0,

        // Initialize room
        initializeRoom: async (roomId, userId, username) => {
          set({ roomId, userId, username, isRoomOwner: userId === null })

          // Load offline queue if exists
          try {
            const savedQueue = await AsyncStorage.getItem(`queue_${roomId}`)
            if (savedQueue) {
              set({ offlineQueue: JSON.parse(savedQueue) })
            }
          } catch (error) {
            console.error('Failed to load offline queue:', error)
          }

          // Initialize OT engine
          set({ otEngine: new OTEngine() })

          // Start sync loop
          get().startSyncLoop()
        },

        // Apply local operations
        applyLocalOperation: async (operation) => {
          const { document, cursorPosition, offlineQueue, otEngine, isOnline } = get()

          try {
            // Apply operation locally
            const newDocument = otEngine.apply(document, operation)

            // Update cursor position
            let newCursorPosition = cursorPosition
            if (operation.type === 'insert') {
              if (operation.position <= cursorPosition) {
                newCursorPosition += operation.length
              }
            } else if (operation.type === 'delete') {
              if (operation.position < cursorPosition) {
                newCursorPosition = Math.max(operation.position, cursorPosition - operation.length)
              }
            }

            set({
              document: newDocument,
              cursorPosition: newCursorPosition,
            })

            // Queue or send operation
            if (isOnline) {
              // Send immediately if online
              try {
                await get().sendOperation(operation)
              } catch (error) {
                console.error('Failed to send operation:', error)
                // Add to queue on error
                const newQueue = [...offlineQueue, operation]
                set({ offlineQueue: newQueue, offlineQueueSize: newQueue.length })
                await AsyncStorage.setItem(`queue_${get().roomId}`, JSON.stringify(newQueue))
              }
            } else {
              // Queue if offline
              const newQueue = [...offlineQueue, operation]
              set({ offlineQueue: newQueue, offlineQueueSize: newQueue.length })
              await AsyncStorage.setItem(`queue_${get().roomId}`, JSON.stringify(newQueue))
            }
          } catch (error) {
            console.error('Failed to apply local operation:', error)
            set({ syncError: error.message })
          }
        },

        // Apply remote operations
        applyRemoteOperation: (operation) => {
          const { document, otEngine, offlineQueue } = get()

          try {
            // Transform against pending operations
            let transformedOp = operation
            for (const pendingOp of offlineQueue) {
              transformedOp = otEngine.transform(transformedOp, pendingOp)
            }

            // Apply transformed operation
            const newDocument = otEngine.apply(document, transformedOp)
            set({ document: newDocument })
          } catch (error) {
            console.error('Failed to apply remote operation:', error)
            set({ syncError: error.message })
          }
        },

        // Send operation to server
        sendOperation: async (operation) => {
          const { roomId, userId } = get()

          const response = await fetch(
            `${process.env.REACT_NATIVE_API_URL}/api/rooms/${roomId}/operations`,
            {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json',
                Authorization: `Bearer ${await getToken()}`,
              },
              body: JSON.stringify({
                userId,
                operation,
                timestamp: Date.now(),
              }),
            }
          )

          if (!response.ok) {
            throw new Error(`Failed to send operation: ${response.statusText}`)
          }

          return response.json()
        },

        // Send message
        sendMessage: async (content) => {
          const { roomId, userId, username } = get()

          const response = await fetch(
            `${process.env.REACT_NATIVE_API_URL}/api/rooms/${roomId}/messages`,
            {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json',
                Authorization: `Bearer ${await getToken()}`,
              },
              body: JSON.stringify({
                userId,
                username,
                content,
                timestamp: Date.now(),
              }),
            }
          )

          if (!response.ok) {
            throw new Error(`Failed to send message: ${response.statusText}`)
          }

          const message = await response.json()
          set({ messages: [...get().messages, message] })
        },

        // Update cursor position
        updateCursor: (userId, position) => {
          const { cursorPositions } = get()
          set({
            cursorPositions: {
              ...cursorPositions,
              [userId]: position,
            },
          })

          // Emit cursor update event
          // This would use WebSocket
        },

        // Set typing indicator
        setTyping: (isTyping) => {
          const { userId, typingUsers } = get()

          if (isTyping) {
            if (!typingUsers.includes(userId)) {
              set({ typingUsers: [...typingUsers, userId] })
            }
          } else {
            set({ typingUsers: typingUsers.filter((id) => id !== userId) })
          }

          // Emit typing event
          // This would use WebSocket
        },

        // Update online status
        setOnlineStatus: (isOnline) => {
          set({ isOnline })

          if (isOnline) {
            // Sync when coming back online
            get().sync()
          }
        },

        // Sync with server
        sync: async () => {
          const { roomId, userId, offlineQueue, isSyncing, isOnline } = get()

          if (isSyncing || !isOnline || !roomId) return

          set({ isSyncing: true })

          try {
            // Sync offline queue
            if (offlineQueue.length > 0) {
              for (const operation of offlineQueue) {
                await get().sendOperation(operation)
              }
              set({ offlineQueue: [], offlineQueueSize: 0 })
              await AsyncStorage.removeItem(`queue_${roomId}`)
            }

            // Fetch latest document
            const response = await fetch(
              `${process.env.REACT_NATIVE_API_URL}/api/rooms/${roomId}/sync`,
              {
                headers: {
                  Authorization: `Bearer ${await getToken()}`,
                },
              }
            )

            if (!response.ok) {
              throw new Error(`Sync failed: ${response.statusText}`)
            }

            const { document, version, operations } = await response.json()

            set({
              document,
              documentVersion: version,
              ackedVersion: version,
              lastSyncTime: Date.now(),
              syncError: null,
            })

            // Process any received operations
            for (const operation of operations || []) {
              get().applyRemoteOperation(operation)
            }
          } catch (error) {
            console.error('Sync error:', error)
            set({ syncError: error.message })
          } finally {
            set({ isSyncing: false })
          }
        },

        // Start sync loop
        startSyncLoop: () => {
          const syncInterval = setInterval(() => {
            const { isOnline, isBatteryLow } = get()
            if (isOnline) {
              // Adjust sync frequency based on battery
              if (!isBatteryLow) {
                get().sync()
              }
            }
          }, 5000) // Sync every 5 seconds

          // Store interval ID for cleanup
          set({ _syncIntervalId: syncInterval })
        },

        // Stop sync loop
        stopSyncLoop: () => {
          const { _syncIntervalId } = get()
          if (_syncIntervalId) {
            clearInterval(_syncIntervalId)
          }
        },

        // Leave room
        leaveRoom: async () => {
          const { roomId, userId } = get()

          try {
            await fetch(
              `${process.env.REACT_NATIVE_API_URL}/api/rooms/${roomId}/members/${userId}`,
              {
                method: 'DELETE',
                headers: {
                  Authorization: `Bearer ${await getToken()}`,
                },
              }
            )
          } catch (error) {
            console.error('Failed to leave room:', error)
          }

          get().stopSyncLoop()

          set({
            roomId: null,
            userId: null,
            username: null,
            document: '',
            collaborators: [],
            messages: [],
            operations: [],
          })
        },

        // Set battery status
        setBatteryStatus: (isBatteryLow) => {
          set({ isBatteryLow })
        },

        // Update network latency
        updateNetworkLatency: (latency) => {
          set({ networkLatency: latency, lastPingTime: Date.now() })
        },
      }),
      {
        name: 'collaboration-mobile-store',
        storage: {
          getItem: async (name) => {
            try {
              const item = await AsyncStorage.getItem(name)
              return item ? JSON.parse(item) : null
            } catch (error) {
              console.error('Failed to get item from AsyncStorage:', error)
              return null
            }
          },
          setItem: async (name, value) => {
            try {
              await AsyncStorage.setItem(name, JSON.stringify(value))
            } catch (error) {
              console.error('Failed to set item in AsyncStorage:', error)
            }
          },
          removeItem: async (name) => {
            try {
              await AsyncStorage.removeItem(name)
            } catch (error) {
              console.error('Failed to remove item from AsyncStorage:', error)
            }
          },
        },
        partialize: (state) => ({
          // Persist only essential state
          document: state.document,
          documentVersion: state.documentVersion,
          offlineQueue: state.offlineQueue,
        }),
      }
    )
  )

export const useCollaborationMobileStore = createCollaborationMobileStore()
