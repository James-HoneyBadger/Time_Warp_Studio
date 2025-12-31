import { describe, it, expect, beforeEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useEditorStore } from '../store/editorStore'

describe('Editor Store', () => {
  beforeEach(() => {
    useEditorStore.setState({
      code: '',
      language: 'BASIC',
      fontSize: 14,
      theme: 'vs-dark',
      cursorPosition: { line: 0, column: 0 },
      unsavedChanges: false,
    })
  })

  it('should initialize with defaults', () => {
    const { result } = renderHook(() => useEditorStore())
    expect(result.current.code).toBe('')
    expect(result.current.language).toBe('BASIC')
    expect(result.current.fontSize).toBe(14)
    expect(result.current.theme).toBe('vs-dark')
    expect(result.current.unsavedChanges).toBe(false)
  })

  it('should set code', () => {
    const { result } = renderHook(() => useEditorStore())
    const code = 'PRINT "Hello, World!"'
    
    act(() => {
      result.current.setCode(code)
    })

    expect(result.current.code).toBe(code)
    expect(result.current.unsavedChanges).toBe(true)
  })

  it('should change language', () => {
    const { result } = renderHook(() => useEditorStore())
    
    act(() => {
      result.current.setLanguage('Python')
    })

    expect(result.current.language).toBe('Python')
  })

  it('should change theme', () => {
    const { result } = renderHook(() => useEditorStore())
    
    act(() => {
      result.current.setTheme('vs-light')
    })

    expect(result.current.theme).toBe('vs-light')
  })

  it('should mark as saved', () => {
    const { result } = renderHook(() => useEditorStore())
    
    act(() => {
      result.current.setCode('some code')
    })

    expect(result.current.unsavedChanges).toBe(true)

    act(() => {
      result.current.markSaved()
    })

    expect(result.current.unsavedChanges).toBe(false)
  })
})
