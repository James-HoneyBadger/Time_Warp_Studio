import { describe, it, expect, beforeEach, vi } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useAuthStore } from '../store/authStore'

describe('Auth Store', () => {
  beforeEach(() => {
    // Clear localStorage before each test
    localStorage.clear()
    useAuthStore.setState({
      user: null,
      token: null,
      isAuthenticated: false,
    })
  })

  it('should initialize with empty state', () => {
    const { result } = renderHook(() => useAuthStore())
    expect(result.current.isAuthenticated).toBe(false)
    expect(result.current.user).toBe(null)
    expect(result.current.token).toBe(null)
  })

  it('should login user', () => {
    const { result } = renderHook(() => useAuthStore())
    
    act(() => {
      result.current.login({ email: 'test@example.com' }, 'token123')
    })

    expect(result.current.isAuthenticated).toBe(true)
    expect(result.current.user?.email).toBe('test@example.com')
    expect(result.current.token).toBe('token123')
  })

  it('should logout user', () => {
    const { result } = renderHook(() => useAuthStore())
    
    act(() => {
      result.current.setUser({ email: 'test@example.com' }, 'token123')
    })

    expect(result.current.isAuthenticated).toBe(true)

    act(() => {
      result.current.logout()
    })

    expect(result.current.isAuthenticated).toBe(false)
    expect(result.current.user).toBe(null)
    expect(result.current.token).toBe(null)
  })

  it('should persist to localStorage', () => {
    const { result } = renderHook(() => useAuthStore())
    
    act(() => {
      result.current.setUser({ email: 'test@example.com' }, 'token123')
    })

    const stored = JSON.parse(localStorage.getItem('auth-storage') || '{}')
    expect(stored.state?.isAuthenticated).toBe(true)
  })
})
