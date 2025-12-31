import React, { useEffect, useState } from 'react'
import { useCollaborationStore } from '../store/collaborationStore'

export const ActivityStream = () => {
  const [displayedActivities, setDisplayedActivities] = useState([])
  const changeHistory = useCollaborationStore((state) => state.changeHistory)
  const collaborators = useCollaborationStore((state) => state.collaborators)

  useEffect(() => {
    // Convert change history to activity items
    const activities = changeHistory.slice(-10).reverse().map((change) => {
      const collaborator = collaborators.find((c) => c.id === change.userId)
      return {
        id: change.id,
        timestamp: change.timestamp,
        userId: change.userId,
        username: collaborator?.name || 'Unknown',
        action: change.type,
        details: change.details,
      }
    })
    setDisplayedActivities(activities)
  }, [changeHistory, collaborators])

  const getActivityIcon = (action) => {
    switch (action) {
      case 'insert':
        return '✏️'
      case 'delete':
        return '🗑️'
      case 'execute':
        return '▶️'
      case 'save':
        return '💾'
      default:
        return '📝'
    }
  }

  const getActivityLabel = (action) => {
    switch (action) {
      case 'insert':
        return 'Inserted code'
      case 'delete':
        return 'Deleted code'
      case 'execute':
        return 'Executed code'
      case 'save':
        return 'Saved file'
      default:
        return 'Made changes'
    }
  }

  const formatTime = (timestamp) => {
    const now = Date.now()
    const diff = now - timestamp
    const seconds = Math.floor(diff / 1000)
    const minutes = Math.floor(seconds / 60)
    const hours = Math.floor(minutes / 60)

    if (seconds < 60) return 'just now'
    if (minutes < 60) return `${minutes}m ago`
    if (hours < 24) return `${hours}h ago`
    return new Date(timestamp).toLocaleDateString()
  }

  return (
    <div className="bg-gray-900 rounded-lg p-4 max-w-sm h-96 overflow-y-auto">
      <h3 className="text-lg font-semibold text-white mb-4 sticky top-0 bg-gray-900">
        Activity
      </h3>

      {displayedActivities.length === 0 ? (
        <div className="text-center py-8">
          <p className="text-gray-400">No activity yet</p>
        </div>
      ) : (
        <div className="space-y-3">
          {displayedActivities.map((activity) => (
            <div
              key={activity.id}
              className="flex items-start gap-3 pb-3 border-b border-gray-700 last:border-b-0"
            >
              <span className="text-lg flex-shrink-0">
                {getActivityIcon(activity.action)}
              </span>
              <div className="flex-1 min-w-0">
                <p className="text-sm text-white">
                  <span className="font-medium">{activity.username}</span>
                  {' '}
                  {getActivityLabel(activity.action)}
                </p>
                {activity.details && (
                  <p className="text-xs text-gray-400 mt-1 truncate">
                    {activity.details}
                  </p>
                )}
                <p className="text-xs text-gray-500 mt-1">
                  {formatTime(activity.timestamp)}
                </p>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

export default ActivityStream
