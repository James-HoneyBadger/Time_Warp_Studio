import React, { useState, useEffect, useRef } from 'react'
import { useChatStore } from '../store/chatStore'

export const ChatPanel = ({ roomId, userId }) => {
  const [input, isTyping] = useState('')
  const messagesEndRef = useRef(null)

  const messages = useChatStore((state) => state.messages)
  const addMessage = useChatStore((state) => state.addMessage)
  const participants = useChatStore((state) => state.participants)

  useEffect(() => {
    // Auto-scroll to latest message
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }, [messages])

  const handleSend = (e) => {
    e.preventDefault()
    if (!input.trim()) return

    const message = {
      id: Date.now(),
      userId,
      username: participants.find((p) => p.id === userId)?.name || 'Unknown',
      content: input,
      timestamp: Date.now(),
      edited: false,
    }

    addMessage(message)
    isTyping('')
  }

  const formatTime = (timestamp) => {
    return new Date(timestamp).toLocaleTimeString([], {
      hour: '2-digit',
      minute: '2-digit',
    })
  }

  return (
    <div className="flex flex-col h-full bg-gray-900 rounded-lg">
      {/* Header */}
      <div className="p-4 border-b border-gray-700">
        <h3 className="text-lg font-semibold text-white">Room Chat</h3>
        <p className="text-xs text-gray-400 mt-1">{participants.length} participants</p>
      </div>

      {/* Messages */}
      <div className="flex-1 overflow-y-auto p-4 space-y-4">
        {messages.length === 0 ? (
          <div className="flex items-center justify-center h-full">
            <p className="text-gray-400">No messages yet. Start the conversation!</p>
          </div>
        ) : (
          messages.map((msg) => (
            <div
              key={msg.id}
              className={`flex gap-3 ${msg.userId === userId ? 'justify-end' : 'justify-start'}`}
            >
              <div
                className={`max-w-xs rounded-lg p-3 ${
                  msg.userId === userId
                    ? 'bg-blue-600 text-white'
                    : 'bg-gray-800 text-gray-100'
                }`}
              >
                {msg.userId !== userId && (
                  <p className="text-xs font-semibold text-gray-300 mb-1">
                    {msg.username}
                  </p>
                )}
                <p className="text-sm break-words">{msg.content}</p>
                {msg.edited && (
                  <p className="text-xs opacity-70 mt-1">(edited)</p>
                )}
                <p className="text-xs opacity-70 mt-1">
                  {formatTime(msg.timestamp)}
                </p>
              </div>
            </div>
          ))
        )}
        <div ref={messagesEndRef} />
      </div>

      {/* Input */}
      <form onSubmit={handleSend} className="p-4 border-t border-gray-700">
        <div className="flex gap-2">
          <input
            type="text"
            value={input}
            onChange={(e) => isTyping(e.target.value)}
            placeholder="Type a message..."
            className="flex-1 bg-gray-800 text-white rounded px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
          />
          <button
            type="submit"
            disabled={!input.trim()}
            className="bg-blue-600 hover:bg-blue-700 disabled:bg-gray-700 text-white rounded px-4 py-2 text-sm font-medium transition"
          >
            Send
          </button>
        </div>
      </form>
    </div>
  )
}

export default ChatPanel
