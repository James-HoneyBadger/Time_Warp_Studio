import { describe, it, expect, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import { BrowserRouter } from 'react-router-dom'
import Navigation from '../components/Navigation'

describe('Navigation Component', () => {
  beforeEach(() => {
    localStorage.clear()
  })

  it('should render navigation', () => {
    render(
      <BrowserRouter>
        <Navigation />
      </BrowserRouter>
    )

    expect(screen.getByText('Time Warp IDE')).toBeInTheDocument()
  })

  it('should show cloud status', () => {
    render(
      <BrowserRouter>
        <Navigation />
      </BrowserRouter>
    )

    // Should show either "Online" or "Offline"
    const status = screen.queryByText(/Online|Offline/)
    expect(status).toBeInTheDocument()
  })
})
