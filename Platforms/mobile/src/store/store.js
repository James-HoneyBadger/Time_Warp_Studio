// Redux store configuration for Time Warp Mobile

import { configureStore } from '@reduxjs/toolkit';
import editorReducer from './slices/editorSlice';
import projectReducer from './slices/projectSlice';
import authReducer from './slices/authSlice';
import cloudReducer from './slices/cloudSlice';
import settingsReducer from './slices/settingsSlice';

const store = configureStore({
  reducer: {
    editor: editorReducer,
    project: projectReducer,
    auth: authReducer,
    cloud: cloudReducer,
    settings: settingsReducer,
  },
});

export default store;
