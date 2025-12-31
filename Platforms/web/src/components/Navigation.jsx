import { useNavigate } from 'react-router-dom'
import { useAuthStore } from '../store/authStore'
import { useCloudStore } from '../store/cloudStore'
import { Menu, LogOut, Settings, Cloud, CloudOff } from 'lucide-react'
import { useState } from 'react'

export default function Navigation() {
  const navigate = useNavigate()
  const { isAuthenticated, user, logout } = useAuthStore()
  const { isOnline } = useCloudStore()
  const [showMenu, setShowMenu] = useState(false)

  const handleLogout = () => {
    logout()
    navigate('/')
  }

  return (
    <nav className="bg-surface border-b border-border px-6 py-3 flex items-center justify-between sticky top-0 z-50">
      {/* Logo / Title */}
      <div
        onClick={() => navigate('/')}
        className="flex items-center gap-2 cursor-pointer hover:opacity-80 transition"
      >
        <div className="w-8 h-8 bg-primary rounded flex items-center justify-center">
          <span className="text-white font-bold">TW</span>
        </div>
        <span className="font-bold text-lg">Time Warp IDE</span>
      </div>

      {/* Cloud Status */}
      <div className="flex items-center gap-4">
        {isOnline ? (
          <div className="flex items-center gap-2 text-green-400 text-sm">
            <Cloud className="w-4 h-4" />
            <span>Online</span>
          </div>
        ) : (
          <div className="flex items-center gap-2 text-yellow-400 text-sm">
            <CloudOff className="w-4 h-4" />
            <span>Offline</span>
          </div>
        )}

        {/* User Menu */}
        <div className="relative">
          <button
            onClick={() => setShowMenu(!showMenu)}
            className="flex items-center gap-2 px-3 py-2 rounded hover:bg-dark-bg transition"
          >
            <Menu className="w-5 h-5" />
          </button>

          {showMenu && (
            <div className="absolute right-0 mt-2 w-48 bg-surface border border-border rounded-lg shadow-lg py-2">
              {isAuthenticated ? (
                <>
                  <div className="px-4 py-2 border-b border-border">
                    <p className="text-sm text-gray-400">Signed in as</p>
                    <p className="font-medium">{user?.email}</p>
                  </div>

                  <button
                    onClick={() => {
                      navigate('/settings')
                      setShowMenu(false)
                    }}
                    className="w-full text-left px-4 py-2 hover:bg-dark-bg flex items-center gap-2 transition"
                  >
                    <Settings className="w-4 h-4" />
                    Settings
                  </button>

                  <button
                    onClick={handleLogout}
                    className="w-full text-left px-4 py-2 hover:bg-dark-bg text-red-400 flex items-center gap-2 transition"
                  >
                    <LogOut className="w-4 h-4" />
                    Logout
                  </button>
                </>
              ) : (
                <button
                  onClick={() => {
                    navigate('/login')
                    setShowMenu(false)
                  }}
                  className="w-full text-left px-4 py-2 hover:bg-dark-bg transition"
                >
                  Login
                </button>
              )}
            </div>
          )}
        </div>
      </div>
    </nav>
  )
}
