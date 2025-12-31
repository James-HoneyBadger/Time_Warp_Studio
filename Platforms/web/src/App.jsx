import { Routes, Route, Navigate } from 'react-router-dom'
import Navigation from './components/Navigation'
import DashboardPage from './pages/DashboardPage'
import EditorPage from './pages/EditorPage'
import SettingsPage from './pages/SettingsPage'
import { useAuthStore } from './store/authStore'

function App() {
  const { isAuthenticated } = useAuthStore()

  return (
    <div className="flex flex-col h-screen bg-dark-bg text-dark-text">
      <Navigation />
      <main className="flex-1 overflow-hidden">
        <Routes>
          {isAuthenticated ? (
            <>
              <Route path="/" element={<DashboardPage />} />
              <Route path="/editor/:projectId" element={<EditorPage />} />
              <Route path="/settings" element={<SettingsPage />} />
              <Route path="*" element={<Navigate to="/" replace />} />
            </>
          ) : (
            <>
              <Route path="/" element={<DashboardPage />} />
              <Route path="*" element={<Navigate to="/" replace />} />
            </>
          )}
        </Routes>
      </main>
    </div>
  )
}

export default App
