import { useState } from 'react'
import { useAuthStore } from '../store/authStore'
import { useEditorStore } from '../store/editorStore'
import { useCloudStore } from '../store/cloudStore'
import { Moon, Sun, LogOut, Save } from 'lucide-react'

export default function SettingsPage() {
  const { user, logout } = useAuthStore()
  const { theme, setTheme, fontSize, setFontSize } = useEditorStore()
  const { isOnline } = useCloudStore()
  const [saved, setSaved] = useState(false)

  const themes = [
    { value: 'vs-dark', label: 'Dark' },
    { value: 'vs-light', label: 'Light' },
    { value: 'vs-code-dark', label: 'VS Code Dark' },
    { value: 'monokai', label: 'Monokai' },
  ]

  const handleSaveSettings = () => {
    setSaved(true)
    setTimeout(() => setSaved(false), 2000)
  }

  const handleLogout = () => {
    logout()
  }

  return (
    <div className="p-8 max-w-2xl mx-auto">
      <h1 className="text-4xl font-bold mb-8">Settings</h1>

      {/* Editor Settings */}
      <section className="bg-surface rounded-lg p-6 mb-6 border border-border">
        <h2 className="text-2xl font-bold mb-4">Editor</h2>
        
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium mb-2">Theme</label>
            <div className="grid grid-cols-2 gap-2">
              {themes.map((t) => (
                <button
                  key={t.value}
                  onClick={() => setTheme(t.value)}
                  className={`p-3 rounded border-2 transition ${
                    theme === t.value
                      ? 'border-primary bg-primary bg-opacity-10'
                      : 'border-border hover:border-primary'
                  }`}
                >
                  <div className="flex items-center gap-2">
                    {t.value.includes('light') ? (
                      <Sun className="w-4 h-4" />
                    ) : (
                      <Moon className="w-4 h-4" />
                    )}
                    {t.label}
                  </div>
                </button>
              ))}
            </div>
          </div>

          <div>
            <label className="block text-sm font-medium mb-2">Font Size</label>
            <div className="flex items-center gap-4">
              <input
                type="range"
                min="10"
                max="24"
                value={fontSize}
                onChange={(e) => setFontSize(parseInt(e.target.value))}
                className="flex-1"
              />
              <span className="text-white w-16">{fontSize}px</span>
            </div>
          </div>

          <div>
            <label className="block text-sm font-medium mb-2">Auto-save</label>
            <label className="flex items-center gap-2 cursor-pointer">
              <input type="checkbox" defaultChecked className="w-4 h-4" />
              <span>Automatically save changes</span>
            </label>
          </div>
        </div>
      </section>

      {/* Account Settings */}
      <section className="bg-surface rounded-lg p-6 mb-6 border border-border">
        <h2 className="text-2xl font-bold mb-4">Account</h2>
        
        <div className="space-y-4">
          <div>
            <span className="text-gray-400">Email:</span>
            <p className="text-white text-lg">{user?.email || 'Not signed in'}</p>
          </div>

          <div>
            <span className="text-gray-400">Cloud Status:</span>
            <p className="text-white text-lg">
              {isOnline ? '🟢 Online' : '🔴 Offline'}
            </p>
          </div>

          <button
            onClick={handleLogout}
            className="w-full px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700 flex items-center justify-center gap-2"
          >
            <LogOut className="w-4 h-4" />
            Logout
          </button>
        </div>
      </section>

      {/* Save Button */}
      <div className="flex gap-2">
        <button
          onClick={handleSaveSettings}
          className="px-6 py-2 bg-primary text-white rounded-lg hover:bg-blue-600 flex items-center gap-2"
        >
          <Save className="w-4 h-4" />
          Save Settings
        </button>
        {saved && <p className="text-green-400">✓ Settings saved</p>}
      </div>
    </div>
  )
}
