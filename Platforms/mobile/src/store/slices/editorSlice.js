// Editor state slice

import { createSlice } from '@reduxjs/toolkit';

const editorSlice = createSlice({
  name: 'editor',
  initialState: {
    code: '',
    language: 'BASIC',
    cursorPosition: 0,
    unsavedChanges: false,
    fontSize: 14,
    indentSize: 2,
    autoSave: true,
  },
  reducers: {
    setCode: (state, action) => {
      state.code = action.payload;
      state.unsavedChanges = true;
    },
    setLanguage: (state, action) => {
      state.language = action.payload;
    },
    setCursorPosition: (state, action) => {
      state.cursorPosition = action.payload;
    },
    markSaved: (state) => {
      state.unsavedChanges = false;
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
  },
});

export const {
  setCode,
  setLanguage,
  setCursorPosition,
  markSaved,
  setFontSize,
  setIndentSize,
  setAutoSave,
} = editorSlice.actions;

export default editorSlice.reducer;
