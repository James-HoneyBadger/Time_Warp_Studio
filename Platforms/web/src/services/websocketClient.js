import io from 'socket.io-client'

class WebSocketClient {
  constructor(url = import.meta.env.VITE_WS_URL || 'http://localhost:8000') {
    this.url = url
    this.socket = null
    this.listeners = new Map()
    this.isConnected = false
    this.reconnectAttempts = 0
    this.maxReconnectAttempts = 5
  }

  connect(token = null) {
    if (this.socket?.connected) {
      return Promise.resolve()
    }

    return new Promise((resolve, reject) => {
      const socketOptions = {
        reconnection: true,
        reconnectionDelay: 1000,
        reconnectionDelayMax: 5000,
        reconnectionAttempts: this.maxReconnectAttempts,
        transports: ['websocket', 'polling'],
      }

      if (token) {
        socketOptions.auth = { token }
      }

      this.socket = io(this.url, socketOptions)

      this.socket.on('connect', () => {
        this.isConnected = true
        this.reconnectAttempts = 0
        this.emit('connected', { connectionId: this.socket.id })
        resolve()
      })

      this.socket.on('disconnect', () => {
        this.isConnected = false
        this.emit('disconnected', { reason: 'socket disconnected' })
      })

      this.socket.on('error', (error) => {
        console.error('WebSocket error:', error)
        this.emit('error', { error })
        reject(error)
      })

      this.socket.on('reconnect_attempt', () => {
        this.reconnectAttempts++
        this.emit('reconnecting', { attempt: this.reconnectAttempts })
      })

      this.socket.on('reconnect_error', (error) => {
        console.error('Reconnect error:', error)
        this.emit('reconnect_error', { error })
      })

      // Timeout after 10 seconds
      setTimeout(() => {
        if (!this.isConnected) {
          reject(new Error('Connection timeout'))
        }
      }, 10000)
    })
  }

  disconnect() {
    if (this.socket) {
      this.socket.disconnect()
      this.socket = null
      this.isConnected = false
    }
  }

  on(event, callback) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, [])

      // Register with socket if connected
      if (this.socket) {
        this.socket.on(event, (...args) => {
          this.listeners.get(event).forEach((cb) => cb(...args))
        })
      }
    }

    const callbacks = this.listeners.get(event)
    callbacks.push(callback)

    // Return unsubscribe function
    return () => {
      const index = callbacks.indexOf(callback)
      if (index > -1) {
        callbacks.splice(index, 1)
      }
    }
  }

  off(event, callback) {
    if (this.listeners.has(event)) {
      const callbacks = this.listeners.get(event)
      const index = callbacks.indexOf(callback)
      if (index > -1) {
        callbacks.splice(index, 1)
      }
    }
  }

  emit(event, data) {
    if (this.socket?.connected) {
      this.socket.emit(event, data)
    } else {
      console.warn(`Cannot emit ${event}: socket not connected`)
    }
  }

  once(event) {
    return new Promise((resolve) => {
      const handler = (data) => {
        this.off(event, handler)
        resolve(data)
      }
      this.on(event, handler)
    })
  }

  // Collaboration-specific methods

  joinRoom(roomId, userId, userData) {
    this.emit('join_room', { roomId, userId, userData })
  }

  leaveRoom(roomId) {
    this.emit('leave_room', { roomId })
  }

  sendChange(change) {
    this.emit('code_change', change)
  }

  sendCursorUpdate(position) {
    this.emit('cursor_update', position)
  }

  sendPresenceUpdate(status) {
    this.emit('presence_update', status)
  }

  sendChatMessage(message) {
    this.emit('chat_message', message)
  }

  // Typing indicator
  notifyTyping(isTyping = true) {
    this.emit('typing', { isTyping })
  }

  // Ack-based messaging
  emitWithAck(event, data) {
    return new Promise((resolve, reject) => {
      if (!this.socket?.connected) {
        reject(new Error('Socket not connected'))
        return
      }

      this.socket.emit(event, data, (response) => {
        if (response?.error) {
          reject(new Error(response.error))
        } else {
          resolve(response)
        }
      })
    })
  }

  // Connection status
  isOnline() {
    return this.isConnected && this.socket?.connected
  }

  getConnectionId() {
    return this.socket?.id
  }

  getReconnectInfo() {
    return {
      isConnected: this.isConnected,
      reconnectAttempts: this.reconnectAttempts,
      maxReconnectAttempts: this.maxReconnectAttempts,
    }
  }
}

// Singleton instance
let instance = null

export const getWebSocketClient = (url) => {
  if (!instance) {
    instance = new WebSocketClient(url)
  }
  return instance
}

export const createWebSocketClient = (url) => {
  instance = new WebSocketClient(url)
  return instance
}

export default WebSocketClient
