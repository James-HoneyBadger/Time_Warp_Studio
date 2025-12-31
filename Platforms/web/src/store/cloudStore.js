import { create } from 'zustand'

export const useCloudStore = create((set) => ({
  isOnline: navigator.onLine,
  isSyncing: false,
  syncStatus: 'idle',
  lastSync: null,
  pendingChanges: [],
  conflicts: [],
  
  setOnlineStatus: (online) => set({ isOnline: online }),
  setSyncing: (syncing) => set({ isSyncing: syncing }),
  setSyncStatus: (status) => set({ syncStatus: status }),
  setLastSync: (time) => set({ lastSync: time }),
  setPendingChanges: (changes) => set({ pendingChanges: changes }),
  setConflicts: (conflicts) => set({ conflicts }),
  
  addPendingChange: (change) => set((state) => ({
    pendingChanges: [...state.pendingChanges, change],
  })),
  
  removePendingChange: (changeId) => set((state) => ({
    pendingChanges: state.pendingChanges.filter(c => c.id !== changeId),
  })),
}))
