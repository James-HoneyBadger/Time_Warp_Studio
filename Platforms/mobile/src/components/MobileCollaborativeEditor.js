/**
 * Mobile Collaborative Editor Component
 * React Native component with touch support
 */

import React, { useRef, useCallback, useState } from 'react'
import {
  View,
  TextInput,
  StyleSheet,
  ScrollView,
  ActivityIndicator,
  Text,
  TouchableOpacity,
  PanResponder,
  GestureResponderEvent,
} from 'react-native'
import { useMobileCollaborativeEditor, useMobileGestures } from '../hooks/useMobileCollaboration'
import MobileGestureService from '../services/gestureService'

const MobileCollaborativeEditor = ({ roomId, userId, username }) => {
  const editor = useMobileCollaborativeEditor(roomId, userId, username)
  const gestures = useMobileGestures()
  const [cursorVisible, setCursorVisible] = useState(true)

  const gestureService = useRef(new MobileGestureService(editor)).current
  const panResponder = useRef(gestureService.createPanResponder()).current

  const handleChange = useCallback(
    (text) => {
      const { document, cursorPosition } = editor

      // Calculate difference
      const diff = text.length - document.length
      if (diff > 0) {
        // Text inserted
        const insertedText = text.substring(cursorPosition, cursorPosition + diff)
        editor.handleInsert(cursorPosition, insertedText)
      } else if (diff < 0) {
        // Text deleted
        editor.handleDelete(cursorPosition + diff, -diff)
      }
    },
    [editor]
  )

  const handleSelectionChange = useCallback((event) => {
    const { start, end } = event.nativeEvent.selection
    // This would update selection state
  }, [])

  if (!editor.roomId) {
    return (
      <View style={styles.container}>
        <ActivityIndicator size="large" color="#0000ff" />
        <Text style={styles.loadingText}>Loading editor...</Text>
      </View>
    )
  }

  return (
    <View style={[styles.container, { backgroundColor: '#fff' }]} {...panResponder.panHandlers}>
      {/* Top toolbar */}
      <View style={styles.toolbar}>
        <Text style={styles.roomName}>{editor.roomId}</Text>

        {/* Sync status */}
        <View style={styles.syncStatus}>
          {editor.isSyncing ? (
            <>
              <ActivityIndicator size="small" color="#0000ff" />
              <Text style={styles.syncText}>Syncing...</Text>
            </>
          ) : (
            <View style={[styles.statusDot, { backgroundColor: editor.isOnline ? '#4CAF50' : '#ff9800' }]} />
          )}
        </View>

        {/* Connection quality indicator */}
        <View style={styles.connectionIndicator}>
          <Text style={styles.connectionText}>{editor.networkType.toUpperCase()}</Text>
        </View>
      </View>

      {/* Editor */}
      <ScrollView style={styles.editorContainer} scrollEnabled={true}>
        <View style={styles.editorContent}>
          <TextInput
            style={styles.textInput}
            value={editor.document}
            onChangeText={handleChange}
            onSelectionChange={handleSelectionChange}
            placeholder="Start typing..."
            placeholderTextColor="#ccc"
            multiline
            scrollEnabled={false}
            onTouchStart={(e) => gestures.handleTouchStart(e)}
            onTouchMove={(e) => gestures.handleTouchMove(e)}
          />

          {/* Collaborator cursors */}
          <CollaboratorCursors collaborators={editor.collaborators} cursorPositions={editor.cursorPositions} />

          {/* Battery warning */}
          {editor.isBatteryLow && (
            <View style={styles.batteryWarning}>
              <Text style={styles.warningText}>⚠️ Low Battery - Sync frequency reduced</Text>
            </View>
          )}
        </View>
      </ScrollView>

      {/* Bottom toolbar */}
      <View style={styles.bottomToolbar}>
        <TouchableOpacity style={styles.toolButton} onPress={() => editor.handleInsert(editor.cursorPosition, '\n')}>
          <Text style={styles.buttonText}>↩️ Enter</Text>
        </TouchableOpacity>

        <TouchableOpacity style={styles.toolButton} onPress={() => gestureService.handleCopy()}>
          <Text style={styles.buttonText}>📋 Copy</Text>
        </TouchableOpacity>

        <TouchableOpacity style={styles.toolButton} onPress={() => gestureService.handlePaste()}>
          <Text style={styles.buttonText}>📌 Paste</Text>
        </TouchableOpacity>

        {/* Offline queue size */}
        {editor.offlineQueueSize > 0 && (
          <View style={styles.queueIndicator}>
            <Text style={styles.queueText}>{editor.offlineQueueSize} pending</Text>
          </View>
        )}
      </View>

      {/* Typing indicator */}
      {editor.typingUsers && editor.typingUsers.length > 0 && (
        <View style={styles.typingIndicator}>
          <Text style={styles.typingText}>{editor.typingUsers.join(', ')} typing...</Text>
        </View>
      )}
    </View>
  )
}

/**
 * Collaborator Cursors Component
 */
const CollaboratorCursors = ({ collaborators, cursorPositions }) => {
  return (
    <View style={styles.cursorsContainer}>
      {collaborators.map((collaborator) => {
        const position = cursorPositions[collaborator.userId] || 0
        const color = collaborator.color || '#0000ff'

        return (
          <View
            key={collaborator.userId}
            style={[styles.collaboratorCursor, { left: position * 8, borderLeftColor: color }]}
          >
            <Text style={[styles.collaboratorLabel, { color }]}>{collaborator.username}</Text>
          </View>
        )
      })}
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#f5f5f5',
  },

  toolbar: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between',
    paddingVertical: 12,
    paddingHorizontal: 16,
    backgroundColor: '#fff',
    borderBottomWidth: 1,
    borderBottomColor: '#e0e0e0',
  },

  roomName: {
    fontSize: 16,
    fontWeight: 'bold',
    flex: 1,
  },

  syncStatus: {
    flexDirection: 'row',
    alignItems: 'center',
    marginHorizontal: 8,
  },

  syncText: {
    fontSize: 12,
    marginLeft: 4,
    color: '#666',
  },

  statusDot: {
    width: 8,
    height: 8,
    borderRadius: 4,
  },

  connectionIndicator: {
    paddingHorizontal: 8,
    paddingVertical: 4,
    backgroundColor: '#e3f2fd',
    borderRadius: 4,
  },

  connectionText: {
    fontSize: 10,
    fontWeight: 'bold',
    color: '#1976d2',
  },

  editorContainer: {
    flex: 1,
    backgroundColor: '#fff',
  },

  editorContent: {
    padding: 12,
  },

  textInput: {
    fontSize: 14,
    fontFamily: 'Courier New',
    padding: 8,
    backgroundColor: '#fafafa',
    borderRadius: 4,
    borderWidth: 1,
    borderColor: '#e0e0e0',
    minHeight: 300,
  },

  cursorsContainer: {
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    pointerEvents: 'none',
  },

  collaboratorCursor: {
    position: 'absolute',
    width: 2,
    height: 20,
    borderLeftWidth: 2,
  },

  collaboratorLabel: {
    fontSize: 10,
    marginTop: -18,
    marginLeft: 4,
    fontWeight: 'bold',
  },

  batteryWarning: {
    marginTop: 12,
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#fff3e0',
    borderRadius: 4,
    borderLeftWidth: 3,
    borderLeftColor: '#ff9800',
  },

  warningText: {
    fontSize: 12,
    color: '#e65100',
  },

  bottomToolbar: {
    flexDirection: 'row',
    justifyContent: 'space-around',
    alignItems: 'center',
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#fff',
    borderTopWidth: 1,
    borderTopColor: '#e0e0e0',
  },

  toolButton: {
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#e3f2fd',
    borderRadius: 4,
    marginHorizontal: 4,
  },

  buttonText: {
    fontSize: 12,
    fontWeight: 'bold',
    color: '#1976d2',
  },

  queueIndicator: {
    paddingVertical: 6,
    paddingHorizontal: 12,
    backgroundColor: '#fff3e0',
    borderRadius: 4,
  },

  queueText: {
    fontSize: 11,
    fontWeight: 'bold',
    color: '#e65100',
  },

  typingIndicator: {
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#f5f5f5',
    borderTopWidth: 1,
    borderTopColor: '#e0e0e0',
  },

  typingText: {
    fontSize: 12,
    color: '#666',
    fontStyle: 'italic',
  },

  loadingText: {
    marginTop: 12,
    fontSize: 14,
    color: '#666',
  },
})

export default MobileCollaborativeEditor
