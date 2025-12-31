import React from 'react'
import { usePresenceStore } from '../store/presenceStore'

export const PresenceList = () => {
  const localUser = usePresenceStore((state) => state.localUser)
  const remoteUsers = usePresenceStore((state) => state.remoteUsers)

  const getStatusColor = (status) => {
    switch (status) {
      case 'editing':
        return 'bg-blue-500'
      case 'running':
        return 'bg-green-500'
      case 'idle':
        return 'bg-gray-400'
      case 'away':
        return 'bg-yellow-500'
      default:
        return 'bg-gray-400'
    }
  }

  const getStatusLabel = (status) => {
    return status.charAt(0).toUpperCase() + status.slice(1)
  }

  const formatTime = (timestamp) => {
    if (!timestamp) return ''
    const now = Date.now()
    const diff = now - timestamp
    const seconds = Math.floor(diff / 1000)
    const minutes = Math.floor(seconds / 60)

    if (seconds < 60) return 'just now'
    if (minutes < 60) return `${minutes}m ago`
    return `${Math.floor(minutes / 60)}h ago`
  }

  return (
    <div className="bg-gray-900 rounded-lg p-4 max-w-sm">
      <h3 className="text-lg font-semibold text-white mb-4">Active Users</h3>

      {/* Local user */}
      {localUser && (
        <div className="mb-4 pb-4 border-b border-gray-700">
          <div className="flex items-center gap-3">
            <div
              className={`w-3 h-3 rounded-full ${getStatusColor(localUser.status)}`}
            ></div>
            <div className="flex-1">
              <p className="text-white font-medium">{localUser.name}</p>
              <p className="text-xs text-gray-400">{getStatusLabel(localUser.status)}</p>
            </div>
            <span className="text-xs text-gray-500">(you)</span>
          </div>
        </div>
      )}

      {/* Remote users */}
      {remoteUsers.length > 0 ? (
        <div className="space-y-3">
          {remoteUsers.map((user) => (
            <div
              key={user.id}
              className="flex items-center gap-3 pb-3 border-b border-gray-700 last:border-b-0"
            >
              <div
                className={`w-3 h-3 rounded-full ${getStatusColor(user.status)}`}
              ></div>
              <div
                className="w-6 h-6 rounded-full text-xs font-semibold flex items-center justify-center text-white"
                style={{ backgroundColor: user.color }}
              >
                {user.name.charAt(0).toUpperCase()}
              </div>
              <div className="flex-1 min-w-0">
                <p className="text-white font-medium truncate">{user.name}</p>
                <p className="text-xs text-gray-400">
                  {getStatusLabel(user.status)} • {formatTime(user.lastActivity)}
                </p>
              </div>
            </div>
          ))}
        </div>
      ) : (
        <p className="text-gray-400 text-sm">No other users online</p>
      )}

      {/* User count */}
      <div className="mt-4 pt-4 border-t border-gray-700">
        <p className="text-xs text-gray-400">
          {remoteUsers.length + 1} user{remoteUsers.length + 1 !== 1 ? 's' : ''} online
        </p>
      </div>
    </div>
  )
}

export default PresenceList
