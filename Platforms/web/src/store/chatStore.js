import { create } from 'zustand'

export const useChatStore = create((set) => ({
  // Messages
  messages: [],
  totalMessages: 0,

  // Chat state
  isLoading: false,
  error: null,
  hasMore: true,

  // Notifications
  unreadCount: 0,
  mentionedInMessage: false,

  // User details
  currentUserId: null,
  participants: [],

  // Actions
  addMessage: (message) =>
    set((state) => ({
      messages: [...state.messages, message],
      totalMessages: state.totalMessages + 1,
      unreadCount:
        message.userId !== state.currentUserId
          ? state.unreadCount + 1
          : state.unreadCount,
    })),

  editMessage: (messageId, content) =>
    set((state) => ({
      messages: state.messages.map((m) =>
        m.id === messageId ? { ...m, content, edited: true, editedAt: Date.now() } : m
      ),
    })),

  deleteMessage: (messageId) =>
    set((state) => ({
      messages: state.messages.filter((m) => m.id !== messageId),
      totalMessages: state.totalMessages - 1,
    })),

  loadMoreMessages: (newMessages) =>
    set((state) => ({
      messages: [...newMessages, ...state.messages],
      hasMore: newMessages.length > 0,
    })),

  setMessages: (messages) =>
    set({
      messages,
      totalMessages: messages.length,
      unreadCount: 0,
    }),

  clearUnread: () => set({ unreadCount: 0 }),

  setMentioned: (mentioned) => set({ mentionedInMessage: mentioned }),

  setCurrentUserId: (userId) => set({ currentUserId: userId }),

  setParticipants: (participants) => set({ participants }),

  addParticipant: (participant) =>
    set((state) => ({
      participants: [...state.participants, participant],
    })),

  removeParticipant: (userId) =>
    set((state) => ({
      participants: state.participants.filter((p) => p.id !== userId),
    })),

  setLoading: (loading) => set({ isLoading: loading }),

  setError: (error) => set({ error }),

  clearChat: () =>
    set({
      messages: [],
      totalMessages: 0,
      unreadCount: 0,
      error: null,
    }),
}))
