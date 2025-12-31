import { describe, it, expect, beforeEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useProjectStore } from '../store/projectStore'

describe('Project Store', () => {
  beforeEach(() => {
    localStorage.clear()
    useProjectStore.setState({
      projects: [],
      currentProject: null,
      files: [],
      selectedFile: null,
      isLoading: false,
      error: null,
    })
  })

  it('should initialize with empty state', () => {
    const { result } = renderHook(() => useProjectStore())
    expect(result.current.projects).toEqual([])
    expect(result.current.currentProject).toBe(null)
    expect(result.current.files).toEqual([])
  })

  it('should set projects', () => {
    const { result } = renderHook(() => useProjectStore())
    const projects = [
      { id: 1, name: 'Project 1', language: 'BASIC' },
      { id: 2, name: 'Project 2', language: 'Python' },
    ]
    
    act(() => {
      result.current.setProjects(projects)
    })

    expect(result.current.projects).toEqual(projects)
  })

  it('should add file', () => {
    const { result } = renderHook(() => useProjectStore())
    const file = { id: 1, name: 'main.bas', content: '' }
    
    act(() => {
      result.current.addFile(file)
    })

    expect(result.current.files).toContain(file)
  })

  it('should remove file', () => {
    const { result } = renderHook(() => useProjectStore())
    const file = { id: 1, name: 'main.bas', content: '' }
    
    act(() => {
      result.current.addFile(file)
    })

    expect(result.current.files.length).toBe(1)

    act(() => {
      result.current.removeFile(1)
    })

    expect(result.current.files.length).toBe(0)
  })

  it('should update file', () => {
    const { result } = renderHook(() => useProjectStore())
    const file = { id: 1, name: 'main.bas', content: 'PRINT "Hello"' }
    
    act(() => {
      result.current.addFile(file)
    })

    const updated = { id: 1, name: 'main.bas', content: 'PRINT "Updated"' }
    
    act(() => {
      result.current.updateFile(1, updated)
    })

    expect(result.current.files[0].content).toBe('PRINT "Updated"')
  })
})
