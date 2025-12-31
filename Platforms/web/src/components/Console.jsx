import { useState } from 'react'
import { Copy, Download, Trash2 } from 'lucide-react'

export default function Console({ output, onClear }) {
  const [copied, setCopied] = useState(false)

  const handleCopy = () => {
    navigator.clipboard.writeText(output)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const handleDownload = () => {
    const element = document.createElement('a')
    element.setAttribute(
      'href',
      'data:text/plain;charset=utf-8,' + encodeURIComponent(output)
    )
    element.setAttribute('download', 'output.txt')
    element.style.display = 'none'
    document.body.appendChild(element)
    element.click()
    document.body.removeChild(element)
  }

  return (
    <div className="flex flex-col h-full bg-dark-bg border-t border-border">
      {/* Console Header */}
      <div className="px-4 py-2 border-b border-border flex items-center justify-between bg-surface">
        <span className="text-sm font-bold text-gray-400">Output</span>
        <div className="flex items-center gap-2">
          <button
            onClick={handleCopy}
            title="Copy output"
            className="p-1 hover:bg-dark-bg rounded transition"
          >
            {copied ? (
              <span className="text-xs text-green-400">✓ Copied</span>
            ) : (
              <Copy className="w-4 h-4 text-gray-400" />
            )}
          </button>
          <button
            onClick={handleDownload}
            title="Download output"
            className="p-1 hover:bg-dark-bg rounded transition"
          >
            <Download className="w-4 h-4 text-gray-400" />
          </button>
          <button
            onClick={onClear}
            title="Clear console"
            className="p-1 hover:bg-dark-bg rounded transition"
          >
            <Trash2 className="w-4 h-4 text-gray-400" />
          </button>
        </div>
      </div>

      {/* Console Output */}
      <div className="flex-1 overflow-auto p-4">
        <div className="text-sm font-mono text-gray-400 whitespace-pre-wrap break-words">
          {output ? (
            <div>
              {output.split('\n').map((line, idx) => (
                <div
                  key={idx}
                  className={
                    line.startsWith('❌')
                      ? 'text-red-400'
                      : line.startsWith('✅')
                      ? 'text-green-400'
                      : line.startsWith('⚠️')
                      ? 'text-yellow-400'
                      : line.startsWith('ℹ️')
                      ? 'text-blue-400'
                      : 'text-gray-400'
                  }
                >
                  {line}
                </div>
              ))}
            </div>
          ) : (
            <span className="text-gray-500 italic">
              Run your code to see output here...
            </span>
          )}
        </div>
      </div>
    </div>
  )
}
