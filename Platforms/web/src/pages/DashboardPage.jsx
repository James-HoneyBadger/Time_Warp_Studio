import { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { useAuthStore } from '../store/authStore'
import { useProjectStore } from '../store/projectStore'
import { Plus, Cloud, Zap } from 'lucide-react'

export default function DashboardPage() {
  const { isAuthenticated, logout } = useAuthStore()
  const { projects } = useProjectStore()
  const navigate = useNavigate()
  const [showNewProject, setShowNewProject] = useState(false)
  const [projectName, setProjectName] = useState('')

  const handleNewProject = () => {
    if (projectName.trim()) {
      // TODO: Create project via API
      setProjectName('')
      setShowNewProject(false)
    }
  }

  return (
    <div className="p-8">
      <div className="max-w-6xl mx-auto">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-4xl font-bold mb-2">Time Warp IDE</h1>
          <p className="text-gray-400">Educational Programming Environment</p>
        </div>

        {/* Quick Stats */}
        <div className="grid grid-cols-3 gap-4 mb-8">
          <div className="bg-surface rounded-lg p-6 border border-border">
            <Zap className="w-8 h-8 text-primary mb-2" />
            <div className="text-3xl font-bold">{projects.length}</div>
            <div className="text-gray-400">Projects</div>
          </div>
          <div className="bg-surface rounded-lg p-6 border border-border">
            <Cloud className="w-8 h-8 text-secondary mb-2" />
            <div className="text-3xl font-bold">
              {isAuthenticated ? 'Synced' : 'Offline'}
            </div>
            <div className="text-gray-400">Cloud Status</div>
          </div>
          <div className="bg-surface rounded-lg p-6 border border-border">
            <Plus className="w-8 h-8 text-info mb-2" />
            <div className="text-3xl font-bold">Ready</div>
            <div className="text-gray-400">To Code</div>
          </div>
        </div>

        {/* Projects List */}
        <div className="bg-surface rounded-lg p-6 border border-border">
          <div className="flex justify-between items-center mb-6">
            <h2 className="text-2xl font-bold">Projects</h2>
            <button
              onClick={() => setShowNewProject(true)}
              className="bg-primary text-white px-4 py-2 rounded-lg hover:bg-blue-600 flex items-center gap-2"
            >
              <Plus className="w-4 h-4" />
              New Project
            </button>
          </div>

          {projects.length === 0 ? (
            <p className="text-gray-400 text-center py-8">
              No projects yet. Create one to get started!
            </p>
          ) : (
            <div className="grid gap-4">
              {projects.map((project) => (
                <div
                  key={project.id}
                  onClick={() => navigate(`/editor/${project.id}`)}
                  className="p-4 bg-dark-bg rounded border border-border hover:border-primary cursor-pointer transition"
                >
                  <h3 className="font-bold">{project.name}</h3>
                  <p className="text-gray-400 text-sm">{project.language}</p>
                </div>
              ))}
            </div>
          )}
        </div>

        {/* New Project Modal */}
        {showNewProject && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center">
            <div className="bg-surface rounded-lg p-6 w-96 border border-border">
              <h2 className="text-2xl font-bold mb-4">New Project</h2>
              <input
                type="text"
                placeholder="Project name"
                value={projectName}
                onChange={(e) => setProjectName(e.target.value)}
                className="w-full px-4 py-2 mb-4 bg-dark-bg border border-border rounded text-white"
                onKeyDown={(e) => e.key === 'Enter' && handleNewProject()}
              />
              <div className="flex gap-2">
                <button
                  onClick={handleNewProject}
                  className="flex-1 bg-primary text-white py-2 rounded hover:bg-blue-600"
                >
                  Create
                </button>
                <button
                  onClick={() => setShowNewProject(false)}
                  className="flex-1 bg-gray-700 text-white py-2 rounded hover:bg-gray-600"
                >
                  Cancel
                </button>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
