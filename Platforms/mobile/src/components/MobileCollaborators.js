/**
 * Mobile Collaborators Component
 * Displays active users and their presence status
 */

import React, { useState, useCallback } from 'react'
import {
  View,
  FlatList,
  StyleSheet,
  Text,
  TouchableOpacity,
  Modal,
  ActivityIndicator,
} from 'react-native'
import { useMobilePresence } from '../hooks/useMobileCollaboration'

const MobileCollaborators = ({ userId, username, roomId }) => {
  const presence = useMobilePresence(userId, username)
  const [showDetails, setShowDetails] = useState(false)
  const [selectedCollaborator, setSelectedCollaborator] = useState(null)

  const handleCollaboratorPress = useCallback((collaborator) => {
    setSelectedCollaborator(collaborator)
    setShowDetails(true)
  }, [])

  const getStatusColor = (status) => {
    switch (status) {
      case 'active':
        return '#4CAF50'
      case 'away':
        return '#ff9800'
      case 'offline':
        return '#999'
      default:
        return '#999'
    }
  }

  const getStatusText = (status) => {
    switch (status) {
      case 'active':
        return 'Active'
      case 'away':
        return 'Away'
      case 'offline':
        return 'Offline'
      default:
        return 'Unknown'
    }
  }

  const renderCollaborator = ({ item }) => {
    const isCurrentUser = item.userId === userId
    const statusColor = getStatusColor(item.status)

    return (
      <TouchableOpacity
        style={styles.collaboratorItem}
        onPress={() => handleCollaboratorPress(item)}
        activeOpacity={0.7}
      >
        <View style={styles.collaboratorContent}>
          <View
            style={[
              styles.avatar,
              {
                backgroundColor: isCurrentUser ? '#2196F3' : '#9C27B0',
              },
            ]}
          >
            <Text style={styles.avatarText}>
              {item.username.charAt(0).toUpperCase()}
            </Text>
          </View>

          <View style={styles.collaboratorInfo}>
            <View style={styles.nameContainer}>
              <Text style={styles.collaboratorName}>
                {item.username}
                {isCurrentUser && ' (You)'}
              </Text>

              <View
                style={[
                  styles.statusIndicator,
                  { backgroundColor: statusColor },
                ]}
              />
            </View>

            <Text style={styles.statusText}>
              {getStatusText(item.status)}
              {item.lastSeen && ` • ${formatTime(item.lastSeen)}`}
            </Text>

            {item.isEditing && (
              <View style={styles.editingBadge}>
                <Text style={styles.editingText}>✏️ Editing</Text>
              </View>
            )}

            {item.isTyping && (
              <View style={styles.typingBadge}>
                <Text style={styles.typingText}>💬 Typing</Text>
              </View>
            )}
          </View>

          {item.cursorPosition !== undefined && (
            <Text style={styles.cursorInfo}>Pos: {item.cursorPosition}</Text>
          )}
        </View>

        {item.role === 'owner' && (
          <View style={styles.ownerBadge}>
            <Text style={styles.ownerText}>Owner</Text>
          </View>
        )}

        {item.role === 'editor' && !isCurrentUser && (
          <View style={styles.editorBadge}>
            <Text style={styles.editorText}>Editor</Text>
          </View>
        )}

        {item.role === 'viewer' && !isCurrentUser && (
          <View style={styles.viewerBadge}>
            <Text style={styles.viewerText}>Viewer</Text>
          </View>
        )}
      </TouchableOpacity>
    )
  }

  const renderCollaboratorDetails = () => {
    if (!selectedCollaborator) return null

    return (
      <Modal
        visible={showDetails}
        transparent={true}
        animationType="slide"
        onRequestClose={() => setShowDetails(false)}
      >
        <View style={styles.detailsContainer}>
          {/* Header */}
          <View style={styles.detailsHeader}>
            <TouchableOpacity onPress={() => setShowDetails(false)}>
              <Text style={styles.closeButton}>✕</Text>
            </TouchableOpacity>
            <Text style={styles.detailsTitle}>Collaborator Details</Text>
            <View style={{ width: 24 }} />
          </View>

          {/* Details */}
          <View style={styles.detailsContent}>
            {/* Large avatar */}
            <View
              style={[
                styles.largeAvatar,
                {
                  backgroundColor: selectedCollaborator.userId === userId ? '#2196F3' : '#9C27B0',
                },
              ]}
            >
              <Text style={styles.largeAvatarText}>
                {selectedCollaborator.username.charAt(0).toUpperCase()}
              </Text>
            </View>

            {/* User info */}
            <Text style={styles.detailsUsername}>{selectedCollaborator.username}</Text>

            <View style={styles.detailsInfoBox}>
              <DetailRow
                label="Status"
                value={getStatusText(selectedCollaborator.status)}
              />
              <DetailRow
                label="Role"
                value={selectedCollaborator.role || 'Member'}
              />
              <DetailRow
                label="Cursor Position"
                value={selectedCollaborator.cursorPosition?.toString() || 'N/A'}
              />
              <DetailRow
                label="Last Seen"
                value={
                  selectedCollaborator.lastSeen
                    ? new Date(selectedCollaborator.lastSeen).toLocaleString()
                    : 'N/A'
                }
              />
              <DetailRow
                label="Color"
                value={selectedCollaborator.color || '#000'}
              />
            </View>

            {/* Activity indicators */}
            <View style={styles.activityContainer}>
              <ActivityIndicator
                animating={selectedCollaborator.isEditing}
                size="small"
                color="#2196F3"
                style={{ marginRight: 8 }}
              />
              <Text style={styles.activityText}>
                {selectedCollaborator.isEditing ? 'Currently editing' : 'Not editing'}
              </Text>
            </View>

            {/* Permissions */}
            <View style={styles.permissionsContainer}>
              <Text style={styles.permissionsTitle}>Permissions</Text>

              <PermissionRow
                label="Can view document"
                granted={true}
              />
              <PermissionRow
                label="Can edit document"
                granted={selectedCollaborator.role === 'owner' || selectedCollaborator.role === 'editor'}
              />
              <PermissionRow
                label="Can invite others"
                granted={selectedCollaborator.role === 'owner'}
              />
              <PermissionRow
                label="Can remove members"
                granted={selectedCollaborator.role === 'owner'}
              />
            </View>
          </View>

          {/* Close button */}
          <TouchableOpacity
            style={styles.detailsCloseButton}
            onPress={() => setShowDetails(false)}
          >
            <Text style={styles.detailsCloseButtonText}>Close</Text>
          </TouchableOpacity>
        </View>
      </Modal>
    )
  }

  return (
    <View style={styles.container}>
      {/* Header */}
      <View style={styles.header}>
        <Text style={styles.headerTitle}>Collaborators</Text>
        <View style={styles.countBadge}>
          <Text style={styles.countText}>{presence.collaborators?.length || 0}</Text>
        </View>
      </View>

      {/* List of collaborators */}
      <FlatList
        data={presence.collaborators}
        renderItem={renderCollaborator}
        keyExtractor={(item) => item.userId}
        contentContainerStyle={styles.listContent}
        scrollEnabled={false}
      />

      {/* No collaborators message */}
      {(!presence.collaborators || presence.collaborators.length === 0) && (
        <View style={styles.emptyState}>
          <Text style={styles.emptyStateText}>No other collaborators yet</Text>
          <Text style={styles.emptyStateSubtext}>Invite others to collaborate</Text>
        </View>
      )}

      {/* Details modal */}
      {renderCollaboratorDetails()}
    </View>
  )
}

/**
 * Detail Row Component
 */
const DetailRow = ({ label, value }) => (
  <View style={styles.detailRow}>
    <Text style={styles.detailLabel}>{label}:</Text>
    <Text style={styles.detailValue}>{value}</Text>
  </View>
)

/**
 * Permission Row Component
 */
const PermissionRow = ({ label, granted }) => (
  <View style={styles.permissionRow}>
    <Text style={styles.permissionLabel}>{label}</Text>
    <Text style={[styles.permissionStatus, granted && styles.grantedStatus]}>
      {granted ? '✓' : '✗'}
    </Text>
  </View>
)

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#f5f5f5',
  },

  header: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingVertical: 12,
    paddingHorizontal: 16,
    backgroundColor: '#fff',
    borderBottomWidth: 1,
    borderBottomColor: '#e0e0e0',
  },

  headerTitle: {
    fontSize: 18,
    fontWeight: 'bold',
  },

  countBadge: {
    paddingVertical: 4,
    paddingHorizontal: 12,
    backgroundColor: '#e3f2fd',
    borderRadius: 12,
  },

  countText: {
    fontSize: 12,
    fontWeight: 'bold',
    color: '#1976d2',
  },

  listContent: {
    paddingVertical: 8,
  },

  collaboratorItem: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between',
    paddingVertical: 12,
    paddingHorizontal: 16,
    marginVertical: 4,
    marginHorizontal: 8,
    backgroundColor: '#fff',
    borderRadius: 8,
    borderLeftWidth: 3,
    borderLeftColor: '#2196F3',
  },

  collaboratorContent: {
    flexDirection: 'row',
    alignItems: 'center',
    flex: 1,
  },

  avatar: {
    width: 40,
    height: 40,
    borderRadius: 20,
    justifyContent: 'center',
    alignItems: 'center',
    marginRight: 12,
  },

  avatarText: {
    color: '#fff',
    fontWeight: 'bold',
    fontSize: 16,
  },

  collaboratorInfo: {
    flex: 1,
  },

  nameContainer: {
    flexDirection: 'row',
    alignItems: 'center',
  },

  collaboratorName: {
    fontSize: 14,
    fontWeight: 'bold',
    marginRight: 8,
  },

  statusIndicator: {
    width: 8,
    height: 8,
    borderRadius: 4,
  },

  statusText: {
    fontSize: 12,
    color: '#999',
    marginTop: 2,
  },

  editingBadge: {
    marginTop: 4,
    paddingVertical: 2,
    paddingHorizontal: 8,
    backgroundColor: '#c8e6c9',
    borderRadius: 4,
    alignSelf: 'flex-start',
  },

  editingText: {
    fontSize: 10,
    color: '#1b5e20',
    fontWeight: '500',
  },

  typingBadge: {
    marginTop: 4,
    paddingVertical: 2,
    paddingHorizontal: 8,
    backgroundColor: '#e3f2fd',
    borderRadius: 4,
    alignSelf: 'flex-start',
  },

  typingText: {
    fontSize: 10,
    color: '#1565c0',
    fontWeight: '500',
  },

  cursorInfo: {
    fontSize: 10,
    color: '#666',
    marginLeft: 8,
  },

  ownerBadge: {
    paddingVertical: 4,
    paddingHorizontal: 8,
    backgroundColor: '#fff3e0',
    borderRadius: 4,
    marginLeft: 8,
  },

  ownerText: {
    fontSize: 10,
    fontWeight: 'bold',
    color: '#e65100',
  },

  editorBadge: {
    paddingVertical: 4,
    paddingHorizontal: 8,
    backgroundColor: '#c8e6c9',
    borderRadius: 4,
    marginLeft: 8,
  },

  editorText: {
    fontSize: 10,
    fontWeight: 'bold',
    color: '#1b5e20',
  },

  viewerBadge: {
    paddingVertical: 4,
    paddingHorizontal: 8,
    backgroundColor: '#e0e0e0',
    borderRadius: 4,
    marginLeft: 8,
  },

  viewerText: {
    fontSize: 10,
    fontWeight: 'bold',
    color: '#616161',
  },

  emptyState: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    paddingVertical: 32,
  },

  emptyStateText: {
    fontSize: 16,
    fontWeight: 'bold',
    color: '#999',
  },

  emptyStateSubtext: {
    fontSize: 12,
    color: '#ccc',
    marginTop: 8,
  },

  // Details modal styles
  detailsContainer: {
    flex: 1,
    backgroundColor: '#fff',
  },

  detailsHeader: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingVertical: 16,
    paddingHorizontal: 16,
    borderBottomWidth: 1,
    borderBottomColor: '#e0e0e0',
    backgroundColor: '#f5f5f5',
  },

  closeButton: {
    fontSize: 24,
    color: '#999',
    fontWeight: 'bold',
  },

  detailsTitle: {
    fontSize: 18,
    fontWeight: 'bold',
  },

  detailsContent: {
    flex: 1,
    paddingVertical: 24,
    paddingHorizontal: 16,
  },

  largeAvatar: {
    width: 80,
    height: 80,
    borderRadius: 40,
    justifyContent: 'center',
    alignItems: 'center',
    alignSelf: 'center',
    marginBottom: 16,
  },

  largeAvatarText: {
    color: '#fff',
    fontWeight: 'bold',
    fontSize: 32,
  },

  detailsUsername: {
    fontSize: 20,
    fontWeight: 'bold',
    textAlign: 'center',
    marginBottom: 16,
  },

  detailsInfoBox: {
    backgroundColor: '#f5f5f5',
    borderRadius: 8,
    paddingVertical: 12,
    marginBottom: 16,
  },

  detailRow: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    paddingVertical: 8,
    paddingHorizontal: 12,
    borderBottomWidth: 1,
    borderBottomColor: '#e0e0e0',
  },

  detailLabel: {
    fontSize: 12,
    fontWeight: 'bold',
    color: '#666',
  },

  detailValue: {
    fontSize: 12,
    color: '#000',
  },

  activityContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 12,
    paddingHorizontal: 12,
    backgroundColor: '#e3f2fd',
    borderRadius: 8,
    marginBottom: 16,
  },

  activityText: {
    fontSize: 12,
    color: '#1565c0',
    fontWeight: '500',
  },

  permissionsContainer: {
    backgroundColor: '#f5f5f5',
    borderRadius: 8,
    paddingVertical: 12,
    marginBottom: 16,
  },

  permissionsTitle: {
    fontSize: 12,
    fontWeight: 'bold',
    color: '#666',
    paddingHorizontal: 12,
    marginBottom: 8,
  },

  permissionRow: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingVertical: 8,
    paddingHorizontal: 12,
    borderBottomWidth: 1,
    borderBottomColor: '#e0e0e0',
  },

  permissionLabel: {
    fontSize: 12,
    color: '#000',
  },

  permissionStatus: {
    fontSize: 14,
    color: '#999',
    fontWeight: 'bold',
  },

  grantedStatus: {
    color: '#4CAF50',
  },

  detailsCloseButton: {
    marginVertical: 16,
    marginHorizontal: 16,
    paddingVertical: 12,
    backgroundColor: '#2196F3',
    borderRadius: 8,
    justifyContent: 'center',
    alignItems: 'center',
  },

  detailsCloseButtonText: {
    color: '#fff',
    fontWeight: 'bold',
    fontSize: 14,
  },
})

// Helper function to format time
const formatTime = (timestamp) => {
  const now = Date.now()
  const diff = now - timestamp

  if (diff < 60000) return 'Just now'
  if (diff < 3600000) return `${Math.floor(diff / 60000)}m ago`
  if (diff < 86400000) return `${Math.floor(diff / 3600000)}h ago`
  return `${Math.floor(diff / 86400000)}d ago`
}

export default MobileCollaborators
