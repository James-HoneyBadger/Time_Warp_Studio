/**
 * React Native Hooks for Mobile Collaboration
 * Optimized for touch, battery, and network efficiency
 */

import { useEffect, useCallback, useRef, useState } from 'react'
import { AppState, useWindowDimensions } from 'react-native'
import { useCollaborationIntegrationStore } from '../store/collaborationIntegrationStore'

/**
 * Hook for mobile collaborative editing
 * Optimized for touch input and battery efficiency
 */
export const useMobileCollaborativeEditor = (roomId, userId, username) => {
  const store = useCollaborationIntegrationStore()
  const appState = useRef(AppState.currentState)
  const [isBatteryLow, setIsBatteryLow] = useState(false)
  const [networkType, setNetworkType] = useState('wifi')
  const syncTimeoutRef = useRef(null)
  const { width, height } = useWindowDimensions()

  // Initialize room on mount
  useEffect(() => {
    if (roomId && userId && username) {
      store.initializeRoom(roomId, userId, username).catch(console.error)
    }

    return () => {
      store.leaveRoom()
    }
  }, [roomId, userId, username])

  // Handle app state changes (background/foreground)
  useEffect(() => {
    const subscription = AppState.addEventListener('change', handleAppStateChange)

    return () => {
      subscription.remove()
    }
  }, [])

  const handleAppStateChange = useCallback((nextAppState) => {
    if (appState.current.match(/inactive|background/) && nextAppState === 'active') {
      // App has come to foreground
      store.sync()
      store.setOnlineStatus(true)
    } else if (nextAppState.match(/inactive|background/)) {
      // App has gone to background - reduce sync frequency
      if (syncTimeoutRef.current) {
        clearInterval(syncTimeoutRef.current)
      }
    }
    appState.current = nextAppState
  }, [store])

  // Adaptive sync based on battery and network
  useEffect(() => {
    if (isBatteryLow) {
      // Sync less frequently when battery is low
      syncTimeoutRef.current = setInterval(() => {
        if (store.isOnline && roomId) {
          store.sync()
        }
      }, 15000) // 15 seconds instead of 5
    } else {
      // Normal sync interval
      syncTimeoutRef.current = setInterval(() => {
        if (store.isOnline && roomId) {
          store.sync()
        }
      }, 5000) // 5 seconds
    }

    return () => {
      if (syncTimeoutRef.current) clearInterval(syncTimeoutRef.current)
    }
  }, [isBatteryLow, store.isOnline, roomId])

  // Detect network changes
  useEffect(() => {
    // This would integrate with react-native-netinfo
    // For now, we rely on the store's online/offline status
  }, [networkType])

  const handleInsert = useCallback(
    (position, content) => {
      store.applyLocalOperation({
        type: 'insert',
        position,
        content,
        length: content.length,
      }).catch(console.error)
    },
    [store]
  )

  const handleDelete = useCallback(
    (position, length) => {
      store.applyLocalOperation({
        type: 'delete',
        position,
        length,
      }).catch(console.error)
    },
    [store]
  )

  return {
    ...store,
    handleInsert,
    handleDelete,
    isBatteryLow,
    networkType,
    screenDimensions: { width, height },
    isBackgroundMode: appState.current.match(/inactive|background/),
  }
}

/**
 * Hook for mobile gesture-based cursor control
 */
export const useMobileGestures = () => {
  const store = useCollaborationIntegrationStore()
  const [touchPosition, setTouchPosition] = useState({ x: 0, y: 0 })
  const [pinchScale, setPinchScale] = useState(1)
  const touchTimeoutRef = useRef(null)

  const handleTouchStart = useCallback((e) => {
    const { nativeEvent } = e
    setTouchPosition({
      x: nativeEvent.touches[0].pageX,
      y: nativeEvent.touches[0].pageY,
    })

    // Send cursor position
    store.updateCursor('local', nativeEvent.touches[0].pageX)
  }, [store])

  const handleTouchMove = useCallback((e) => {
    const { nativeEvent } = e

    // Throttle cursor updates to reduce network traffic
    if (touchTimeoutRef.current) {
      clearTimeout(touchTimeoutRef.current)
    }

    touchTimeoutRef.current = setTimeout(() => {
      store.updateCursor('local', nativeEvent.touches[0].pageX)
    }, 100) // Update cursor every 100ms max
  }, [store])

  const handlePinchGesture = useCallback((scale) => {
    setPinchScale(scale)
    // Could use for zoom in editor
  }, [])

  return {
    touchPosition,
    pinchScale,
    handleTouchStart,
    handleTouchMove,
    handlePinchGesture,
  }
}

/**
 * Hook for mobile chat with optimized scrolling
 */
export const useMobileChat = () => {
  const store = useCollaborationIntegrationStore()
  const [isComposing, setIsComposing] = useState(false)
  const [draftMessage, setDraftMessage] = useState('')
  const composingTimeoutRef = useRef(null)

  const handleTyping = useCallback((text) => {
    setDraftMessage(text)
    setIsComposing(true)

    // Notify others
    store.setTyping(true)

    // Reset typing indicator after inactivity
    if (composingTimeoutRef.current) {
      clearTimeout(composingTimeoutRef.current)
    }

    composingTimeoutRef.current = setTimeout(() => {
      store.setTyping(false)
      setIsComposing(false)
    }, 3000)
  }, [store])

  const handleSendMessage = useCallback(async () => {
    if (draftMessage.trim()) {
      try {
        await store.sendMessage(draftMessage)
        setDraftMessage('')
        setIsComposing(false)
      } catch (error) {
        console.error('Failed to send message:', error)
      }
    }
  }, [store, draftMessage])

  return {
    messages: store.messages,
    typingUsers: store.typingUsers,
    draftMessage,
    isComposing,
    handleTyping,
    handleSendMessage,
  }
}

/**
 * Hook for monitoring network status on mobile
 */
export const useMobileNetworkStatus = () => {
  const store = useCollaborationIntegrationStore()
  const [isSlowNetwork, setIsSlowNetwork] = useState(false)
  const [connectionQuality, setConnectionQuality] = useState('good')

  useEffect(() => {
    // This would integrate with react-native-netinfo
    // For now, we track sync errors as network quality indicator
    if (store.syncError) {
      setConnectionQuality('poor')
    } else if (store.isSyncing) {
      setConnectionQuality('fair')
    } else {
      setConnectionQuality('good')
    }
  }, [store.syncError, store.isSyncing])

  return {
    isOnline: store.isOnline,
    isSlowNetwork,
    connectionQuality,
    offlineQueueSize: store.offlineQueueSize,
    lastSyncTime: store.lastSyncTime,
  }
}

/**
 * Hook for managing mobile presence
 */
export const useMobilePresence = (userId, username) => {
  const store = useCollaborationIntegrationStore()
  const [userStatus, setUserStatus] = useState('active')
  const statusTimeoutRef = useRef(null)

  useEffect(() => {
    // Update presence on mount
    store.setTyping(false)

    // Cleanup on unmount
    return () => {
      if (statusTimeoutRef.current) {
        clearTimeout(statusTimeoutRef.current)
      }
    }
  }, [])

  const updatePresence = useCallback((status) => {
    setUserStatus(status)
    // Emit presence event
    // This would use WebSocket
  }, [])

  const handleUserInactive = useCallback(() => {
    updatePresence('away')

    // Auto-reactivate after 1 minute of inactivity
    if (statusTimeoutRef.current) {
      clearTimeout(statusTimeoutRef.current)
    }

    statusTimeoutRef.current = setTimeout(() => {
      updatePresence('offline')
    }, 60000)
  }, [])

  const handleUserActive = useCallback(() => {
    if (statusTimeoutRef.current) {
      clearTimeout(statusTimeoutRef.current)
    }
    updatePresence('active')
  }, [])

  return {
    collaborators: store.collaborators,
    userStatus,
    updatePresence,
    handleUserInactive,
    handleUserActive,
  }
}

/**
 * Hook for mobile accessibility features
 */
export const useMobileAccessibility = () => {
  const [screenReaderEnabled, setScreenReaderEnabled] = useState(false)
  const [largeTextEnabled, setLargeTextEnabled] = useState(false)

  useEffect(() => {
    // Check accessibility settings from native layer
    // This would use react-native-accessibility-engine
  }, [])

  return {
    screenReaderEnabled,
    largeTextEnabled,
    setScreenReaderEnabled,
    setLargeTextEnabled,
  }
}

export default {
  useMobileCollaborativeEditor,
  useMobileGestures,
  useMobileChat,
  useMobileNetworkStatus,
  useMobilePresence,
  useMobileAccessibility,
}
