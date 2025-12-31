import { create } from 'zustand'
import { persist } from 'zustand/middleware'

export const useCollaborationStore = create(
  persist(
    (set) => ({
      // Connection state
      isConnected: false,
      connectionId: null,
      sessionId: null,
      
      // Collaborators
      collaborators: [], // [{ id, name, email, color, cursorPosition, selection }]
      activeUsers: 0,
      
      // Editing state
      pendingChanges: [],
      changeHistory: [],
      lastSyncTime: null,
      isSyncing: false,
      syncError: null,
      
      // Conflict handling
      conflicts: [],
      conflictResolution: 'ot', // operational transform
      
      // Actions
      setConnected: (connected) => set({ isConnected: connected }),
      setConnectionId: (id) => set({ connectionId: id }),
      setSessionId: (id) => set({ sessionId: id }),
      
      addCollaborator: (collaborator) =>
        set((state) => ({
          collaborators: [...state.collaborators, collaborator],
          activeUsers: state.collaborators.length + 1,
        })),
      
      removeCollaborator: (collaboratorId) =>
        set((state) => ({
          collaborators: state.collaborators.filter((c) => c.id !== collaboratorId),
          activeUsers: Math.max(0, state.activeUsers - 1),
        })),
      
      updateCollaboratorCursor: (collaboratorId, position, selection) =>
        set((state) => ({
          collaborators: state.collaborators.map((c) =>
            c.id === collaboratorId ? { ...c, cursorPosition: position, selection } : c
          ),
        })),
      
      addPendingChange: (change) =>
        set((state) => ({
          pendingChanges: [...state.pendingChanges, change],
        })),
      
      clearPendingChanges: () =>
        set({ pendingChanges: [] }),
      
      addToHistory: (change) =>
        set((state) => ({
          changeHistory: [...state.changeHistory, change].slice(-100), // Keep last 100
        })),
      
      setSyncing: (syncing) => set({ isSyncing: syncing }),
      setLastSyncTime: (time) => set({ lastSyncTime: time }),
      setSyncError: (error) => set({ syncError: error }),
      
      addConflict: (conflict) =>
        set((state) => ({
          conflicts: [...state.conflicts, conflict],
        })),
      
      resolveConflict: (conflictId, resolution) =>
        set((state) => ({
          conflicts: state.conflicts.filter((c) => c.id !== conflictId),
        })),
      
      clearAll: () =>
        set({
          isConnected: false,
          connectionId: null,
          sessionId: null,
          collaborators: [],
          activeUsers: 0,
          pendingChanges: [],
          changeHistory: [],
          conflicts: [],
          syncError: null,
        }),
    }),
    {
      name: 'collaboration-storage',
      partialize: (state) => ({
        changeHistory: state.changeHistory,
        lastSyncTime: state.lastSyncTime,
      }),
    }
  )
)
