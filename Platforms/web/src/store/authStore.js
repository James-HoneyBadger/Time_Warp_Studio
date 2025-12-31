import { create } from 'zustand'
import { persist } from 'zustand/middleware'

export const useAuthStore = create(
  persist(
    (set) => ({
      user: null,
      token: null,
      isAuthenticated: false,
      
      login: (email, password) => {
        // TODO: Call API
        set({ 
          user: { email },
          isAuthenticated: true 
        })
      },
      
      logout: () => {
        set({
          user: null,
          token: null,
          isAuthenticated: false,
        })
      },
      
      setUser: (user, token) => {
        set({
          user,
          token,
          isAuthenticated: !!user,
        })
      },
    }),
    { name: 'auth-store' }
  )
)
