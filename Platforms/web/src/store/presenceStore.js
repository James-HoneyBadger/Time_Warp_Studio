import { create } from 'zustand'

export const usePresenceStore = create((set) => ({
  // User presence state
  localUser: {
    id: null,
    name: null,
    email: null,
    color: '#3b82f6',
    status: 'idle', // idle, editing, running, away
    cursorPosition: { line: 0, column: 0 },
    lastActivity: null,
  },

  remoteUsers: [], // [{ id, name, email, color, status, cursorPosition, lastActivity }]

  // Room state
  roomId: null,
  roomName: null,
  roomPrivacy: 'private', // private, shared, public
  roomPermissions: {
    canEdit: true,
    canExecute: true,
    canChat: true,
  },

  // Actions
  setLocalUser: (user) =>
    set((state) => ({
      localUser: { ...state.localUser, ...user },
    })),

  setLocalUserStatus: (status) =>
    set((state) => ({
      localUser: { ...state.localUser, status, lastActivity: Date.now() },
    })),

  setLocalUserCursor: (position) =>
    set((state) => ({
      localUser: { ...state.localUser, cursorPosition: position },
    })),

  addRemoteUser: (user) =>
    set((state) => ({
      remoteUsers: [...state.remoteUsers, user],
    })),

  removeRemoteUser: (userId) =>
    set((state) => ({
      remoteUsers: state.remoteUsers.filter((u) => u.id !== userId),
    })),

  updateRemoteUser: (userId, updates) =>
    set((state) => ({
      remoteUsers: state.remoteUsers.map((u) =>
        u.id === userId ? { ...u, ...updates, lastActivity: Date.now() } : u
      ),
    })),

  setRoomInfo: (roomId, roomName, privacy, permissions) =>
    set({
      roomId,
      roomName,
      roomPrivacy: privacy,
      roomPermissions: permissions,
    }),

  clearPresence: () =>
    set({
      localUser: {
        id: null,
        name: null,
        email: null,
        color: '#3b82f6',
        status: 'idle',
        cursorPosition: { line: 0, column: 0 },
        lastActivity: null,
      },
      remoteUsers: [],
      roomId: null,
      roomName: null,
    }),
}))
