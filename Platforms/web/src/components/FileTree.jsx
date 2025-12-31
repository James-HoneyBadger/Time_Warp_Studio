import { ChevronDown, ChevronRight, File, Folder } from 'lucide-react'
import { useState } from 'react'
import { useProjectStore } from '../store/projectStore'

export default function FileTree({ files, projectId }) {
  const { selectedFile, setSelectedFile } = useProjectStore()
  const [expandedFolders, setExpandedFolders] = useState({})

  const toggleFolder = (folderName) => {
    setExpandedFolders((prev) => ({
      ...prev,
      [folderName]: !prev[folderName],
    }))
  }

  const handleSelectFile = (file) => {
    setSelectedFile(file)
  }

  // Group files by folder
  const groupedFiles = {}
  files?.forEach((file) => {
    const folder = file.path?.split('/')[0] || 'root'
    if (!groupedFiles[folder]) {
      groupedFiles[folder] = []
    }
    groupedFiles[folder].push(file)
  })

  return (
    <div className="p-4">
      <h3 className="text-sm font-bold mb-3 text-gray-400 uppercase">Files</h3>

      {Object.entries(groupedFiles).map(([folder, fileList]) => (
        <div key={folder}>
          {folder !== 'root' && (
            <button
              onClick={() => toggleFolder(folder)}
              className="flex items-center gap-1 w-full p-1 hover:bg-dark-bg rounded transition text-sm"
            >
              {expandedFolders[folder] ? (
                <ChevronDown className="w-4 h-4" />
              ) : (
                <ChevronRight className="w-4 h-4" />
              )}
              <Folder className="w-4 h-4 text-blue-400" />
              <span>{folder}</span>
            </button>
          )}

          {(expandedFolders[folder] !== false || folder === 'root') && (
            <div className={folder !== 'root' ? 'ml-4' : ''}>
              {fileList.map((file) => (
                <button
                  key={file.id}
                  onClick={() => handleSelectFile(file)}
                  className={`flex items-center gap-2 w-full p-2 rounded text-sm transition ${
                    selectedFile?.id === file.id
                      ? 'bg-primary bg-opacity-20 text-primary'
                      : 'hover:bg-dark-bg text-gray-300'
                  }`}
                >
                  <File className="w-3 h-3" />
                  <span className="truncate">{file.name}</span>
                </button>
              ))}
            </div>
          )}
        </div>
      ))}

      {!files || files.length === 0 && (
        <p className="text-xs text-gray-500 italic">No files</p>
      )}
    </div>
  )
}
