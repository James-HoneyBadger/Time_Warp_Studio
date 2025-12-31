// Project state slice

import { createSlice } from '@reduxjs/toolkit';

const projectSlice = createSlice({
  name: 'project',
  initialState: {
    currentProject: null,
    projects: [],
    files: [],
    selectedFile: null,
    loading: false,
    error: null,
  },
  reducers: {
    setCurrentProject: (state, action) => {
      state.currentProject = action.payload;
    },
    setProjects: (state, action) => {
      state.projects = action.payload;
    },
    setFiles: (state, action) => {
      state.files = action.payload;
    },
    setSelectedFile: (state, action) => {
      state.selectedFile = action.payload;
    },
    addFile: (state, action) => {
      state.files.push(action.payload);
    },
    removeFile: (state, action) => {
      state.files = state.files.filter(f => f.id !== action.payload);
    },
    updateFile: (state, action) => {
      const index = state.files.findIndex(f => f.id === action.payload.id);
      if (index !== -1) {
        state.files[index] = action.payload;
      }
    },
    setLoading: (state, action) => {
      state.loading = action.payload;
    },
    setError: (state, action) => {
      state.error = action.payload;
    },
  },
});

export const {
  setCurrentProject,
  setProjects,
  setFiles,
  setSelectedFile,
  addFile,
  removeFile,
  updateFile,
  setLoading,
  setError,
} = projectSlice.actions;

export default projectSlice.reducer;
