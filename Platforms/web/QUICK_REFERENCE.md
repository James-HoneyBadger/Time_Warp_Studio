# Time Warp Web IDE - Quick Reference Guide

## Start Here 🚀

### First Time Setup
```bash
cd Platforms/web
npm install
cp .env.example .env.local
npm run dev
```

Visit: **http://localhost:5173**

## Common Tasks

### Add a New Page
1. Create `src/pages/MyPage.jsx`
2. Add route to `src/App.jsx`:
```jsx
<Route path="/my-page" element={<MyPage />} />
```

### Add a New Component
1. Create `src/components/MyComponent.jsx`
2. Import where needed:
```jsx
import MyComponent from '../components/MyComponent'
```

### Use State Management
```jsx
import { useAuthStore } from '../store/authStore'

function MyComponent() {
  const { isAuthenticated, user } = useAuthStore()
  return <div>{user?.email}</div>
}
```

### Make API Calls
```jsx
import { projectAPI } from '../services/apiClient'

const projects = await projectAPI.list()
```

### Add Styling
- Use Tailwind classes: `className="px-4 py-2 bg-primary"`
- Custom styles in `src/globals.css`

### Write Tests
```javascript
import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import MyComponent from './MyComponent'

describe('MyComponent', () => {
  it('should render', () => {
    render(<MyComponent />)
    expect(screen.getByText('Hello')).toBeInTheDocument()
  })
})
```

## Directory Quick Map

| Path | Purpose |
|------|---------|
| `src/components/` | Reusable UI components |
| `src/pages/` | Page components (routes) |
| `src/store/` | Zustand state stores |
| `src/services/` | API calls, utilities |
| `src/__tests__/` | Test files |
| `src/globals.css` | Global styles |
| `index.html` | HTML template |
| `vite.config.js` | Build config |

## Command Cheat Sheet

```bash
npm run dev              # Start dev server
npm run build            # Production build
npm run preview          # Preview build locally
npm run test             # Run tests
npm run lint             # Check code quality
npm run format           # Format code
npm run type-check       # Check TypeScript
npm run analyze          # Bundle analysis
```

## Styling Quick Reference

### Colors
- Primary: `bg-primary`, `text-primary`
- Secondary: `bg-secondary`, `text-secondary`
- Info: `bg-info`, `text-info`
- Success: `bg-success`, `text-success`
- Error: `bg-error`, `text-error`
- Background: `bg-dark-bg`
- Surface: `bg-surface`
- Border: `border-border`

### Common Classes
```jsx
// Buttons
<button className="px-4 py-2 bg-primary text-white rounded hover:bg-blue-600">
  Click me
</button>

// Cards
<div className="bg-surface rounded-lg p-6 border border-border">
  Content
</div>

// Forms
<input className="bg-dark-bg text-white border border-border rounded px-3 py-2" />
```

## State Management Cheat Sheet

### Auth Store
```javascript
const { user, isAuthenticated, login, logout } = useAuthStore()
```

### Editor Store
```javascript
const { code, setCode, language, theme, fontSize } = useEditorStore()
```

### Project Store
```javascript
const { projects, currentProject, files } = useProjectStore()
```

### Cloud Store
```javascript
const { isOnline, isSyncing, syncStatus } = useCloudStore()
```

## Environment Variables

```env
VITE_API_URL=http://localhost:8000/api
VITE_APP_NAME=Time Warp IDE
VITE_APP_VERSION=6.0.0
VITE_ENABLE_PWA=true
VITE_AUTO_SAVE_INTERVAL=30000
```

## Debugging Tips

### Check Store State
```javascript
import { useAuthStore } from '../store/authStore'

// In browser console:
useAuthStore.getState()
```

### View Network Requests
- Open DevTools → Network tab
- Check requests to API server

### Debug Components
- React DevTools Chrome extension
- Check component props and state

## Common Issues

### Port 5173 in use
```bash
kill -9 $(lsof -t -i:5173)
npm run dev
```

### Dependencies missing
```bash
rm -rf node_modules package-lock.json
npm install
```

### Module not found
- Check import path spelling
- Ensure file exists
- Clear node_modules cache

## File Structure Template

```
Platforms/web/
├── src/
│   ├── components/     # UI components
│   ├── pages/          # Route pages
│   ├── store/          # Zustand stores
│   ├── services/       # API & utilities
│   ├── __tests__/      # Tests
│   ├── App.jsx         # Root
│   ├── main.js         # Entry
│   └── globals.css     # Styles
├── index.html
├── package.json
├── vite.config.js
├── tailwind.config.js
└── README.md
```

## Useful Links

- **React**: https://react.dev
- **Vite**: https://vitejs.dev
- **Tailwind**: https://tailwindcss.com
- **Zustand**: https://github.com/pmndrs/zustand
- **Monaco Editor**: https://microsoft.github.io/monaco-editor/
- **Vitest**: https://vitest.dev

## Getting Help

1. Check documentation in `docs/` folder
2. Read `DEVELOPMENT.md` for detailed guides
3. Check existing tests for usage examples
4. Create an issue: https://github.com/Time-Warp-Studio/Time_Warp_Studio/issues

---

**Last Updated**: 2025  
**Version**: 6.0.0  
**Maintainer**: james@honey-badger.org
