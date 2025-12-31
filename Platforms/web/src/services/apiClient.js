import axios from 'axios'
import { useAuthStore } from '../store/authStore'

const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000/api'

const apiClient = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
})

// Add auth token to requests
apiClient.interceptors.request.use((config) => {
  const { token } = useAuthStore.getState()
  if (token) {
    config.headers.Authorization = `Bearer ${token}`
  }
  return config
})

// Handle auth errors
apiClient.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response?.status === 401) {
      const { logout } = useAuthStore.getState()
      logout()
    }
    return Promise.reject(error)
  }
)

// Auth endpoints
export const authAPI = {
  login: (email, password) =>
    apiClient.post('/auth/login', { email, password }),
  register: (email, password) =>
    apiClient.post('/auth/register', { email, password }),
  refresh: () => apiClient.post('/auth/refresh'),
  logout: () => apiClient.post('/auth/logout'),
}

// Project endpoints
export const projectAPI = {
  list: () => apiClient.get('/projects'),
  get: (id) => apiClient.get(`/projects/${id}`),
  create: (name, language) =>
    apiClient.post('/projects', { name, language }),
  update: (id, data) =>
    apiClient.put(`/projects/${id}`, data),
  delete: (id) => apiClient.delete(`/projects/${id}`),
}

// File endpoints
export const fileAPI = {
  list: (projectId) => apiClient.get(`/projects/${projectId}/files`),
  get: (projectId, fileId) =>
    apiClient.get(`/projects/${projectId}/files/${fileId}`),
  create: (projectId, name, content) =>
    apiClient.post(`/projects/${projectId}/files`, { name, content }),
  update: (projectId, fileId, content) =>
    apiClient.put(`/projects/${projectId}/files/${fileId}`, { content }),
  delete: (projectId, fileId) =>
    apiClient.delete(`/projects/${projectId}/files/${fileId}`),
}

// Execution endpoints
export const executionAPI = {
  run: (projectId, code, language) =>
    apiClient.post(`/execute`, { projectId, code, language }),
  cancel: (executionId) =>
    apiClient.post(`/execute/${executionId}/cancel`),
}

// Cloud sync endpoints
export const cloudAPI = {
  sync: (projectId) => apiClient.post(`/sync`, { projectId }),
  status: (projectId) => apiClient.get(`/sync/status/${projectId}`),
  resolveConflict: (projectId, fileId, resolution) =>
    apiClient.post(`/sync/resolve`, { projectId, fileId, resolution }),
}

// Collaboration endpoints (Phase 4.5.2+)
export const collaborationAPI = {
  // Room operations
  createRoom: (name, isPrivate = false) => {
    const { user } = useAuthStore.getState()
    return apiClient.post(`/api/rooms?owner_id=${user?.id}`, {
      name,
      is_private: isPrivate,
    })
  },
  getRoom: (roomId) => apiClient.get(`/api/rooms/${roomId}`),
  deleteRoom: (roomId) => apiClient.delete(`/api/rooms/${roomId}`),
  getRoomMembers: (roomId) => apiClient.get(`/api/rooms/${roomId}/members`),
  addRoomMember: (roomId, userId, username) =>
    apiClient.post(`/api/rooms/${roomId}/members?user_id=${userId}&user_name=${username}`),
  removeRoomMember: (roomId, userId) =>
    apiClient.delete(`/api/rooms/${roomId}/members/${userId}`),

  // User operations
  getUserProfile: () => {
    const { user } = useAuthStore.getState()
    return apiClient.get(`/api/users/${user?.id}/profile`)
  },
  getUserRooms: () => {
    const { user } = useAuthStore.getState()
    return apiClient.get(`/api/users/${user?.id}/rooms`)
  },
  joinRoom: (roomId, username) => {
    const { user } = useAuthStore.getState()
    return apiClient.post(`/api/users/${user?.id}/rooms/${roomId}/join?username=${username}`)
  },
  leaveRoom: (roomId) => {
    const { user } = useAuthStore.getState()
    return apiClient.post(`/api/users/${user?.id}/rooms/${roomId}/leave`)
  },

  // Sync operations
  recordOperation: (roomId, opType, position, content) => {
    const { user } = useAuthStore.getState()
    return apiClient.post(`/api/sync/${roomId}/operations`, {
      user_id: user?.id,
      op_type: opType,
      position,
      content,
    })
  },
  getCurrentVersion: (roomId) => apiClient.get(`/api/sync/${roomId}/version`),
  getOperations: (roomId, fromVersion = 0, toVersion = null) => {
    let path = `/api/sync/${roomId}/operations?from_version=${fromVersion}`
    if (toVersion !== null) {
      path += `&to_version=${toVersion}`
    }
    return apiClient.get(path)
  },
  getSnapshot: (roomId, version = null) => {
    let path = `/api/sync/${roomId}/snapshot`
    if (version !== null) {
      path += `?version=${version}`
    }
    return apiClient.get(path)
  },
  createSnapshot: (roomId, content, version) =>
    apiClient.post(`/api/sync/${roomId}/snapshot`, {
      content,
      version,
    }),
  fullSync: (roomId, clientVersion) => {
    const { user } = useAuthStore.getState()
    return apiClient.post('/api/sync/sync', {
      room_id: roomId,
      user_id: user?.id,
      client_version: clientVersion,
    })
  },

  // Chat operations
  getRoomMessages: (roomId, limit = 50, offset = 0) =>
    apiClient.get(`/api/rooms/${roomId}/messages?limit=${limit}&offset=${offset}`),
  searchMessages: (roomId, query) =>
    apiClient.get(`/api/rooms/${roomId}/messages/search?query=${encodeURIComponent(query)}`),

  // Health check
  healthCheck: () => apiClient.get('/health'),
}

export default apiClient
