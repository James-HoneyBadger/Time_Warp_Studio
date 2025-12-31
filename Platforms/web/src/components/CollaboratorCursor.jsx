import React, { useEffect } from 'react'
import { usePresenceStore } from '../store/presenceStore'

export const CollaboratorCursor = ({ collaborator }) => {
  if (!collaborator?.cursorPosition) {
    return null
  }

  const { line, column } = collaborator.cursorPosition

  return (
    <div
      className="absolute pointer-events-none"
      style={{
        left: `${column * 8}px`,
        top: `${line * 20}px`,
        zIndex: 50,
      }}
    >
      <div
        className="w-0.5 h-5 animate-pulse"
        style={{ backgroundColor: collaborator.color }}
      >
        <div className="text-xs px-1 py-0.5 bg-gray-900 text-white rounded whitespace-nowrap">
          {collaborator.name}
        </div>
      </div>
    </div>
  )
}

export const CollaboratorCursors = () => {
  const remoteUsers = usePresenceStore((state) => state.remoteUsers)

  return (
    <div className="relative">
      {remoteUsers.map((user) => (
        <CollaboratorCursor key={user.id} collaborator={user} />
      ))}
    </div>
  )
}
