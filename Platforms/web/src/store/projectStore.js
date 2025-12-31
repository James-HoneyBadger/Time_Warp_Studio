import { create } from 'zustand'
import { persist } from 'zustand/middleware'

export const useProjectStore = create(
  persist(
    (set) => ({
      projects: [],
      currentProject: null,
      files: [],
      selectedFile: null,
      isLoading: false,
      error: null,
      
      setProjects: (projects) => set({ projects }),
      setCurrentProject: (project) => set({ currentProject: project }),
      setFiles: (files) => set({ files }),
      setSelectedFile: (file) => set({ selectedFile: file }),
      setLoading: (loading) => set({ isLoading: loading }),
      setError: (error) => set({ error }),
      
      addFile: (file) => set((state) => ({ 
        files: [...state.files, file] 
      })),
      
      removeFile: (fileId) => set((state) => ({
        files: state.files.filter(f => f.id !== fileId),
      })),
      
      updateFile: (fileId, content) => set((state) => ({
        files: state.files.map(f => 
          f.id === fileId ? { ...f, content } : f
        ),
      })),
    }),
    { name: 'project-store' }
  )
)
