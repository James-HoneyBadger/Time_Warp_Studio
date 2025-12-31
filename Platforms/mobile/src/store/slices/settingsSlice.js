// Settings state slice

import { createSlice } from '@reduxjs/toolkit';

const settingsSlice = createSlice({
  name: 'settings',
  initialState: {
    theme: 'dark',
    fontSize: 14,
    indentSize: 2,
    autoSave: true,
    autoSync: true,
    syncInterval: 30,
    notifications: true,
    debugMode: false,
  },
  reducers: {
    setTheme: (state, action) => {
      state.theme = action.payload;
    },
    setFontSize: (state, action) => {
      state.fontSize = action.payload;
    },
    setIndentSize: (state, action) => {
      state.indentSize = action.payload;
    },
    setAutoSave: (state, action) => {
      state.autoSave = action.payload;
    },
    setAutoSync: (state, action) => {
      state.autoSync = action.payload;
    },
    setSyncInterval: (state, action) => {
      state.syncInterval = action.payload;
    },
    setNotifications: (state, action) => {
      state.notifications = action.payload;
    },
    setDebugMode: (state, action) => {
      state.debugMode = action.payload;
    },
  },
});

export const {
  setTheme,
  setFontSize,
  setIndentSize,
  setAutoSave,
  setAutoSync,
  setSyncInterval,
  setNotifications,
  setDebugMode,
} = settingsSlice.actions;

export default settingsSlice.reducer;
