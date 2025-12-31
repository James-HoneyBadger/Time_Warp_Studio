/**
 * Mobile Collaborative Chat Component
 * React Native component optimized for chat on mobile
 */

import React, { useRef, useEffect, useState, useCallback } from 'react'
import {
  View,
  FlatList,
  TextInput,
  StyleSheet,
  TouchableOpacity,
  Text,
  KeyboardAvoidingView,
  Platform,
  ActivityIndicator,
  Image,
} from 'react-native'
import { useMobileChat } from '../hooks/useMobileCollaboration'

const MobileCollaborativeChat = ({ userId, username, roomId }) => {
  const chat = useMobileChat()
  const [scrollToBottom, setScrollToBottom] = useState(false)
  const flatListRef = useRef(null)
  const messageCache = useRef({})

  // Auto-scroll to bottom when new messages arrive
  useEffect(() => {
    if (chat.messages && chat.messages.length > 0) {
      flatListRef.current?.scrollToEnd({ animated: true })
    }
  }, [chat.messages])

  // Handle message sending with optimistic update
  const handleSendMessage = useCallback(async () => {
    if (!chat.draftMessage.trim()) return

    // Create optimistic message
    const optimisticMessage = {
      id: `optimistic_${Date.now()}`,
      userId,
      username,
      content: chat.draftMessage,
      timestamp: Date.now(),
      status: 'sending',
    }

    // Add to cache for immediate display
    messageCache.current[optimisticMessage.id] = optimisticMessage

    // Send message
    try {
      await chat.handleSendMessage()
    } catch (error) {
      console.error('Failed to send message:', error)
      // Mark as failed
      optimisticMessage.status = 'failed'
    }
  }, [chat, userId, username])

  const renderMessageBubble = ({ item }) => {
    const isOwnMessage = item.userId === userId
    const messageTime = new Date(item.timestamp).toLocaleTimeString([], {
      hour: '2-digit',
      minute: '2-digit',
    })

    return (
      <View style={[styles.messageContainer, isOwnMessage && styles.ownMessageContainer]}>
        {!isOwnMessage && <View style={styles.avatarPlaceholder} />}

        <View style={[styles.messageBubble, isOwnMessage && styles.ownMessageBubble]}>
          {!isOwnMessage && (
            <Text style={styles.messageAuthor}>{item.username}</Text>
          )}

          <Text style={[styles.messageText, isOwnMessage && styles.ownMessageText]}>
            {item.content}
          </Text>

          <Text style={[styles.messageTime, isOwnMessage && styles.ownMessageTime]}>
            {messageTime}
          </Text>

          {item.status === 'sending' && (
            <ActivityIndicator size="small" color="#999" style={styles.sendingIndicator} />
          )}

          {item.status === 'failed' && (
            <Text style={styles.failedIndicator}>Failed to send</Text>
          )}

          {item.reactions && item.reactions.length > 0 && (
            <View style={styles.reactionsContainer}>
              {item.reactions.map((reaction) => (
                <TouchableOpacity
                  key={`${reaction.emoji}_${reaction.userId}`}
                  style={styles.reactionBubble}
                  onPress={() => console.log('Reaction tapped')}
                >
                  <Text style={styles.reactionEmoji}>{reaction.emoji}</Text>
                  {reaction.count > 1 && <Text style={styles.reactionCount}>{reaction.count}</Text>}
                </TouchableOpacity>
              ))}
            </View>
          )}
        </View>

        {isOwnMessage && <View style={styles.avatarPlaceholder} />}
      </View>
    )
  }

  const renderTypingIndicator = () => {
    if (!chat.typingUsers || chat.typingUsers.length === 0) return null

    const typingNames = chat.typingUsers.join(', ')

    return (
      <View style={styles.typingContainer}>
        <View style={styles.typingDots}>
          <View style={[styles.typingDot, { animation: 'bounce 1.4s infinite 0s' }]} />
          <View style={[styles.typingDot, { animation: 'bounce 1.4s infinite 0.2s' }]} />
          <View style={[styles.typingDot, { animation: 'bounce 1.4s infinite 0.4s' }]} />
        </View>
        <Text style={styles.typingText}>{typingNames} typing...</Text>
      </View>
    )
  }

  return (
    <KeyboardAvoidingView
      behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
      style={styles.container}
    >
      {/* Messages list */}
      <FlatList
        ref={flatListRef}
        data={chat.messages}
        renderItem={renderMessageBubble}
        keyExtractor={(item) => item.id}
        contentContainerStyle={styles.messagesList}
        onEndReachedThreshold={0.5}
        removeClippedSubviews={true}
        maxToRenderPerBatch={10}
        updateCellsBatchingPeriod={50}
      />

      {/* Typing indicator */}
      {renderTypingIndicator()}

      {/* Input area */}
      <View style={styles.inputContainer}>
        <TextInput
          style={styles.textInput}
          placeholder="Type a message..."
          placeholderTextColor="#ccc"
          value={chat.draftMessage}
          onChangeText={chat.handleTyping}
          editable={true}
          scrollEnabled={true}
          multiline={true}
          maxHeight={100}
        />

        <TouchableOpacity
          style={[
            styles.sendButton,
            !chat.draftMessage.trim() && styles.sendButtonDisabled,
          ]}
          onPress={handleSendMessage}
          disabled={!chat.draftMessage.trim()}
        >
          <Text style={styles.sendButtonText}>Send</Text>
        </TouchableOpacity>
      </View>

      {/* Offline indicator */}
      {!chat.isOnline && (
        <View style={styles.offlineIndicator}>
          <Text style={styles.offlineText}>🔴 Offline - Messages will sync when online</Text>
        </View>
      )}
    </KeyboardAvoidingView>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
  },

  messagesList: {
    paddingVertical: 8,
  },

  messageContainer: {
    flexDirection: 'row',
    marginVertical: 4,
    marginHorizontal: 12,
    alignItems: 'flex-end',
  },

  ownMessageContainer: {
    justifyContent: 'flex-end',
  },

  avatarPlaceholder: {
    width: 32,
    height: 32,
    borderRadius: 16,
    backgroundColor: '#e0e0e0',
    marginHorizontal: 8,
  },

  messageBubble: {
    maxWidth: '70%',
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#e3f2fd',
    borderRadius: 12,
    borderBottomLeftRadius: 2,
  },

  ownMessageBubble: {
    backgroundColor: '#c8e6c9',
    borderBottomLeftRadius: 12,
    borderBottomRightRadius: 2,
  },

  messageAuthor: {
    fontSize: 11,
    fontWeight: 'bold',
    color: '#666',
    marginBottom: 4,
  },

  messageText: {
    fontSize: 14,
    color: '#000',
  },

  ownMessageText: {
    color: '#000',
  },

  messageTime: {
    fontSize: 10,
    color: '#999',
    marginTop: 4,
  },

  ownMessageTime: {
    color: '#666',
  },

  sendingIndicator: {
    marginTop: 4,
  },

  failedIndicator: {
    fontSize: 10,
    color: '#f44336',
    marginTop: 4,
  },

  reactionsContainer: {
    flexDirection: 'row',
    marginTop: 8,
    flexWrap: 'wrap',
  },

  reactionBubble: {
    flexDirection: 'row',
    paddingVertical: 2,
    paddingHorizontal: 6,
    backgroundColor: 'rgba(0, 0, 0, 0.05)',
    borderRadius: 12,
    marginRight: 4,
    marginTop: 4,
    alignItems: 'center',
  },

  reactionEmoji: {
    fontSize: 14,
  },

  reactionCount: {
    fontSize: 10,
    marginLeft: 4,
    color: '#666',
  },

  typingContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 8,
    paddingHorizontal: 16,
  },

  typingDots: {
    flexDirection: 'row',
    alignItems: 'center',
    marginRight: 8,
  },

  typingDot: {
    width: 6,
    height: 6,
    borderRadius: 3,
    backgroundColor: '#999',
    marginHorizontal: 2,
  },

  typingText: {
    fontSize: 12,
    color: '#999',
    fontStyle: 'italic',
  },

  inputContainer: {
    flexDirection: 'row',
    alignItems: 'flex-end',
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#f5f5f5',
    borderTopWidth: 1,
    borderTopColor: '#e0e0e0',
  },

  textInput: {
    flex: 1,
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#fff',
    borderRadius: 20,
    borderWidth: 1,
    borderColor: '#e0e0e0',
    fontSize: 14,
    marginRight: 8,
    maxHeight: 100,
  },

  sendButton: {
    paddingVertical: 8,
    paddingHorizontal: 16,
    backgroundColor: '#4CAF50',
    borderRadius: 20,
    justifyContent: 'center',
    alignItems: 'center',
  },

  sendButtonDisabled: {
    backgroundColor: '#ccc',
  },

  sendButtonText: {
    color: '#fff',
    fontWeight: 'bold',
    fontSize: 12,
  },

  offlineIndicator: {
    paddingVertical: 8,
    paddingHorizontal: 12,
    backgroundColor: '#ffcdd2',
    justifyContent: 'center',
    alignItems: 'center',
  },

  offlineText: {
    fontSize: 12,
    color: '#c62828',
    fontWeight: '500',
  },
})

export default MobileCollaborativeChat
