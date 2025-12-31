import { Editor as MonacoEditor } from '@monaco-editor/react'
import { useEditorStore } from '../store/editorStore'

const languageMap = {
  BASIC: 'basic',
  PILOT: 'text',
  Logo: 'text',
  Python: 'python',
  Rust: 'rust',
  C: 'c',
  Pascal: 'pascal',
  Prolog: 'text',
}

export default function Editor({ code, onChange, language, fontSize }) {
  const { theme } = useEditorStore()

  const handleChange = (value) => {
    onChange(value || '')
  }

  return (
    <MonacoEditor
      language={languageMap[language] || 'text'}
      value={code}
      onChange={handleChange}
      theme={theme}
      options={{
        fontSize,
        minimap: { enabled: false },
        wordWrap: 'on',
        scrollBeyondLastLine: false,
        padding: { top: 16, bottom: 16 },
        defaultFormatter: 'text',
        scrollbar: {
          vertical: 'auto',
          horizontal: 'auto',
          useShadows: false,
          verticalSliderSize: 8,
          horizontalSliderSize: 8,
        },
      }}
    />
  )
}
