import { useState } from 'react'
import { useParams } from 'react-router-dom'
import { useEditorStore } from '../store/editorStore'
import { useProjectStore } from '../store/projectStore'
import { useLiveQuery } from 'dexie-react-hooks'
import Editor from '../components/Editor'
import Console from '../components/Console'
import FileTree from '../components/FileTree'
import { ChevronDown, Save, Play } from 'lucide-react'

export default function EditorPage() {
  const { projectId } = useParams()
  const { code, setCode, language, setLanguage, unsavedChanges } = useEditorStore()
  const { currentProject, files } = useProjectStore()
  const [fontSize, setFontSize] = useState(14)
  const [output, setOutput] = useState('')
  const [isRunning, setIsRunning] = useState(false)

  const languages = ['BASIC', 'PILOT', 'Logo', 'Python', 'C', 'Pascal', 'Prolog']

  const handleRun = async () => {
    setIsRunning(true)
    setOutput('🚀 Running code...\n')
    
    try {
      // TODO: Call interpreter service
      setOutput('✅ Execution complete\n')
    } catch (error) {
      setOutput(`❌ Error: ${error.message}\n`)
    }
    
    setIsRunning(false)
  }

  const handleSave = () => {
    // TODO: Save to cloud
    console.log('Saving project:', projectId)
  }

  return (
    <div className="flex flex-col h-screen bg-dark-bg">
      {/* Toolbar */}
      <div className="bg-surface border-b border-border p-4 flex items-center justify-between">
        <div className="flex items-center gap-4">
          <h1 className="text-xl font-bold">{currentProject?.name || 'Untitled'}</h1>
          
          <div className="flex items-center gap-2 bg-dark-bg rounded px-3 py-1">
            <span className="text-gray-400">Language:</span>
            <select
              value={language}
              onChange={(e) => setLanguage(e.target.value)}
              className="bg-dark-bg text-white outline-none cursor-pointer"
            >
              {languages.map((lang) => (
                <option key={lang} value={lang}>
                  {lang}
                </option>
              ))}
            </select>
          </div>

          <div className="flex items-center gap-2 bg-dark-bg rounded px-3 py-1">
            <span className="text-gray-400">Font:</span>
            <input
              type="range"
              min="10"
              max="24"
              value={fontSize}
              onChange={(e) => setFontSize(parseInt(e.target.value))}
              className="w-24"
            />
            <span className="text-white w-8">{fontSize}px</span>
          </div>
        </div>

        <div className="flex items-center gap-2">
          <button
            onClick={handleSave}
            disabled={!unsavedChanges}
            className="px-4 py-2 bg-primary text-white rounded-lg hover:bg-blue-600 disabled:opacity-50 flex items-center gap-2"
          >
            <Save className="w-4 h-4" />
            Save
          </button>
          <button
            onClick={handleRun}
            disabled={isRunning}
            className="px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 disabled:opacity-50 flex items-center gap-2"
          >
            <Play className="w-4 h-4" />
            {isRunning ? 'Running...' : 'Run'}
          </button>
        </div>
      </div>

      {/* Main Editor Area */}
      <div className="flex flex-1 overflow-hidden">
        {/* File Tree */}
        <div className="w-64 bg-surface border-r border-border overflow-auto">
          <FileTree files={files} projectId={projectId} />
        </div>

        {/* Editor and Console */}
        <div className="flex-1 flex flex-col">
          {/* Editor */}
          <div className="flex-1 overflow-auto">
            <Editor
              code={code}
              onChange={setCode}
              language={language}
              fontSize={fontSize}
            />
          </div>

          {/* Console */}
          <div className="h-64 bg-dark-bg border-t border-border overflow-auto">
            <div className="p-4">
              <div className="text-sm font-mono text-gray-400 whitespace-pre-wrap">
                {output}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
