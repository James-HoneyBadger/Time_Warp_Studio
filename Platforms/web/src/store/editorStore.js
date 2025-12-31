import { create } from 'zustand'

export const useEditorStore = create((set) => ({
  code: '',
  language: 'BASIC',
  fontSize: 14,
  theme: 'vs-dark',
  cursorPosition: 0,
  unsavedChanges: false,
  
  setCode: (code) => set({ code, unsavedChanges: true }),
  setLanguage: (language) => set({ language }),
  setFontSize: (fontSize) => set({ fontSize }),
  setTheme: (theme) => set({ theme }),
  setCursorPosition: (position) => set({ cursorPosition: position }),
  markSaved: () => set({ unsavedChanges: false }),
}))
