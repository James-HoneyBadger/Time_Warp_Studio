// Cloud sync state slice

import { createSlice } from '@reduxjs/toolkit';

const cloudSlice = createSlice({
  name: 'cloud',
  initialState: {
    isOnline: false,
    isSyncing: false,
    syncStatus: 'idle',
    lastSync: null,
    pendingChanges: [],
    conflicts: [],
    error: null,
    offlineMode: false,
  },
  reducers: {
    setOnlineStatus: (state, action) => {
      state.isOnline = action.payload;
    },
    setSyncing: (state, action) => {
      state.isSyncing = action.payload;
    },
    setSyncStatus: (state, action) => {
      state.syncStatus = action.payload;
    },
    setLastSync: (state, action) => {
      state.lastSync = action.payload;
    },
    setPendingChanges: (state, action) => {
      state.pendingChanges = action.payload;
    },
    addPendingChange: (state, action) => {
      state.pendingChanges.push(action.payload);
    },
    removePendingChange: (state, action) => {
      state.pendingChanges = state.pendingChanges.filter(
        c => c.id !== action.payload
      );
    },
    setConflicts: (state, action) => {
      state.conflicts = action.payload;
    },
    addConflict: (state, action) => {
      state.conflicts.push(action.payload);
    },
    removeConflict: (state, action) => {
      state.conflicts = state.conflicts.filter(c => c.id !== action.payload);
    },
    setError: (state, action) => {
      state.error = action.payload;
    },
    setOfflineMode: (state, action) => {
      state.offlineMode = action.payload;
    },
  },
});

export const {
  setOnlineStatus,
  setSyncing,
  setSyncStatus,
  setLastSync,
  setPendingChanges,
  addPendingChange,
  removePendingChange,
  setConflicts,
  addConflict,
  removeConflict,
  setError,
  setOfflineMode,
} = cloudSlice.actions;

export default cloudSlice.reducer;
