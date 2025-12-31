import React, { useEffect, useState } from 'react'
import { useParams } from 'react-router-dom'
import { useEditorStore } from '../store/editorStore'
import { useCollaborationStore } from '../store/collaborationStore'
import { usePresenceStore } from '../store/presenceStore'
import { useChatStore } from '../store/chatStore'
import { getWebSocketClient } from '../services/websocketClient'
import Editor from './Editor'
import Console from './Console'
import ChatPanel from './ChatPanel'
import PresenceList from './PresenceList'
import CollaboratorCursors from './CollaboratorCursor'
import ActivityStream from './ActivityStream'

export const CollaborativePage = () => {
  const { roomId } = useParams()
  const [isConnecting, setIsConnecting] = useState(true)
  const [connectionError, setConnectionError] = useState(null)

  // Editor state
  const code = useEditorStore((state) => state.code)
  const updateCode = useEditorStore((state) => state.updateCode)

  // Collaboration state
  const setConnected = useCollaborationStore((state) => state.setConnected)
  const addCollaborator = useCollaborationStore((state) => state.addCollaborator)
  const removeCollaborator = useCollaborationStore((state) => state.removeCollaborator)
  const updateCollaboratorCursor = useCollaborationStore(
    (state) => state.updateCollaboratorCursor
  )
  const addPendingChange = useCollaborationStore((state) => state.addPendingChange)

  // Presence state
  const setLocalUser = usePresenceStore((state) => state.setLocalUser)
  const setLocalUserStatus = usePresenceStore((state) => state.setLocalUserStatus)
  const setLocalUserCursor = usePresenceStore((state) => state.setLocalUserCursor)
  const addRemoteUser = usePresenceStore((state) => state.addRemoteUser)
  const removeRemoteUser = usePresenceStore((state) => state.removeRemoteUser)
  const updateRemoteUser = usePresenceStore((state) => state.updateRemoteUser)
  const setRoomInfo = usePresenceStore((state) => state.setRoomInfo)

  // Chat state
  const setCurrentUserId = useChatStore((state) => state.setCurrentUserId)
  const setParticipants = useChatStore((state) => state.setParticipants)
  const addMessage = useChatStore((state) => state.addMessage)

  // Initialize WebSocket connection
  useEffect(() => {
    const initializeConnection = async () => {
      try {
        const ws = getWebSocketClient()

        // Connect to WebSocket
        await ws.connect()
        console.log('Connected to collaboration server')

        // Setup event listeners
        ws.on('user_joined', (data) => {
          addRemoteUser(data)
          addCollaborator(data)
          addMessage({
            id: Date.now(),
            userId: 'system',
            username: 'System',
            content: `${data.name} joined the room`,
            timestamp: Date.now(),
          })
        })

        ws.on('user_left', (data) => {
          removeRemoteUser(data.userId)
          removeCollaborator(data.userId)
          addMessage({
            id: Date.now(),
            userId: 'system',
            username: 'System',
            content: `${data.name} left the room`,
            timestamp: Date.now(),
          })
        })

        ws.on('cursor_update', (data) => {
          updateCollaboratorCursor(data.userId, data.position)
          updateRemoteUser(data.userId, { cursorPosition: data.position })
        })

        ws.on('presence_update', (data) => {
          updateRemoteUser(data.userId, {
            status: data.status,
            lastActivity: data.timestamp,
          })
        })

        ws.on('code_change', (data) => {
          updateCode(data.content)
          addPendingChange({
            id: data.id,
            type: data.type,
            content: data.content,
            userId: data.userId,
            timestamp: data.timestamp,
          })
        })

        ws.on('chat_message', addMessage)

        ws.on('room_info', (data) => {
          setRoomInfo(data.id, data.name, data.privacy)
          setParticipants(data.participants)
        })

        // Join room
        ws.emit('join_room', {
          roomId,
          userId: 'user-' + Math.random().toString(36).substr(2, 9),
          name: localStorage.getItem('userName') || 'Anonymous',
        })

        setConnected(true)
        setIsConnecting(false)
      } catch (error) {
        console.error('Connection error:', error)
        setConnectionError(error.message)
        setIsConnecting(false)
      }
    }

    initializeConnection()

    // Cleanup
    return () => {
      const ws = getWebSocketClient()
      if (ws.isOnline()) {
        ws.emit('leave_room', { roomId })
      }
    }
  }, [roomId])

  // Update presence when code changes
  useEffect(() => {
    const ws = getWebSocketClient()
    if (ws.isOnline()) {
      setLocalUserStatus('editing')
      ws.emit('presence_update', { status: 'editing' })

      const timer = setTimeout(() => {
        setLocalUserStatus('idle')
        ws.emit('presence_update', { status: 'idle' })
      }, 5000)

      return () => clearTimeout(timer)
    }
  }, [code])

  if (isConnecting) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-950">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-white mx-auto mb-4"></div>
          <p className="text-white">Connecting to collaboration session...</p>
        </div>
      </div>
    )
  }

  if (connectionError) {
    return (
      <div className="flex items-center justify-center h-screen bg-gray-950">
        <div className="text-center max-w-md">
          <h2 className="text-2xl font-bold text-white mb-4">Connection Error</h2>
          <p className="text-red-400 mb-4">{connectionError}</p>
          <button
            onClick={() => window.location.reload()}
            className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded font-medium"
          >
            Retry
          </button>
        </div>
      </div>
    )
  }

  return (
    <div className="h-screen bg-gray-950 flex flex-col">
      {/* Header */}
      <div className="bg-gray-900 border-b border-gray-700 px-4 py-3">
        <h1 className="text-xl font-bold text-white">
          Collaborative Session - {roomId}
        </h1>
      </div>

      {/* Main content */}
      <div className="flex-1 flex gap-4 overflow-hidden p-4">
        {/* Editor section (left) */}
        <div className="flex-1 flex flex-col gap-4 min-w-0">
          <div className="flex-1 bg-gray-900 rounded-lg overflow-hidden border border-gray-700">
            <Editor />
          </div>
          <CollaboratorCursors />
          <div className="bg-gray-900 rounded-lg overflow-hidden border border-gray-700">
            <Console />
          </div>
        </div>

        {/* Right sidebar */}
        <div className="w-80 flex flex-col gap-4 overflow-hidden">
          {/* Presence */}
          <div className="flex-1 overflow-y-auto">
            <PresenceList />
          </div>

          {/* Activity */}
          <div className="flex-1 overflow-y-auto">
            <ActivityStream />
          </div>

          {/* Chat */}
          <div className="h-64 overflow-hidden">
            <ChatPanel roomId={roomId} userId="local-user" />
          </div>
        </div>
      </div>
    </div>
  )
}

export default CollaborativePage
