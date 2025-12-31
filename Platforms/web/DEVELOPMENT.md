# Time Warp Web IDE - Development Guide

## Quick Start

### Prerequisites
- Node.js 18+ (Download from https://nodejs.org/)
- npm or yarn
- Git

### Setup

```bash
# Clone or navigate to the web version
cd Platforms/web

# Install dependencies
npm install

# Copy environment variables
cp .env.example .env.local

# Start development server
npm run dev
```

The IDE will be available at `http://localhost:5173`

## Project Structure

```
Platforms/web/
├── src/
│   ├── components/           # Reusable React components
│   │   ├── Navigation.jsx     # Top navigation bar
│   │   ├── Editor.jsx         # Monaco editor wrapper
│   │   ├── Console.jsx        # Output display
│   │   └── FileTree.jsx       # File browser
│   ├── pages/                # Page components
│   │   ├── DashboardPage.jsx # Project dashboard
│   │   ├── EditorPage.jsx    # Main editor interface
│   │   └── SettingsPage.jsx  # User settings
│   ├── store/                # Zustand state stores
│   │   ├── authStore.js      # Authentication state
│   │   ├── editorStore.js    # Editor UI state
│   │   ├── projectStore.js   # Projects & files
│   │   └── cloudStore.js     # Cloud sync state
│   ├── services/             # API & utilities
│   │   ├── apiClient.js      # Axios HTTP client
│   │   ├── storage.js        # IndexedDB wrapper (Dexie)
│   │   └── interpreter.js    # Code execution service
│   ├── __tests__/            # Test files
│   ├── App.jsx               # Root component
│   ├── main.js               # Entry point
│   ├── globals.css           # Global styles
│   └── index.css             # CSS imports
├── index.html                # HTML template
├── package.json              # Dependencies
├── vite.config.js            # Build configuration
├── vitest.config.js          # Test configuration
├── tailwind.config.js        # Tailwind CSS config
├── tsconfig.json             # TypeScript config
├── .eslintrc.cjs             # ESLint rules
├── .prettierrc                # Prettier formatting
├── Dockerfile                # Container configuration
├── docker-compose.yml        # Multi-container setup
├── vercel.json               # Vercel deployment config
└── README.md                 # User documentation
```

## Available Scripts

```bash
# Development
npm run dev              # Start development server with hot reload

# Building
npm run build            # Create optimized production build
npm run preview          # Preview production build locally

# Code Quality
npm run type-check       # Run TypeScript type checking
npm run lint             # Check code with ESLint
npm run format           # Format code with Prettier
npm run analyze          # Analyze bundle size

# Testing
npm run test             # Run tests with Vitest
npm test -- --coverage   # Run tests with coverage report

# Utilities
npm run clean            # Remove build artifacts
```

## Styling

### Tailwind CSS

The project uses Tailwind CSS for styling. Custom theme colors are defined in `tailwind.config.js`:

- **Primary**: `#3b82f6` (Blue)
- **Secondary**: `#8b5cf6` (Purple)
- **Info**: `#06b6d4` (Cyan)
- **Success**: `#10b981` (Green)
- **Warning**: `#f59e0b` (Amber)
- **Error**: `#ef4444` (Red)
- **Dark Background**: `#1e1e2e`
- **Surface**: `#2d2d3d`
- **Border**: `#44444d`

### Custom Classes

Edit `src/globals.css` to add custom styles. Use Tailwind `@apply` directive:

```css
.custom-button {
  @apply px-4 py-2 bg-primary text-white rounded hover:bg-blue-600;
}
```

## State Management

### Zustand Stores

The project uses Zustand for state management. Each store is a collection of related state and actions.

#### Auth Store (`src/store/authStore.js`)
```javascript
import { useAuthStore } from './store/authStore'

function MyComponent() {
  const { isAuthenticated, user, login, logout } = useAuthStore()
  // ...
}
```

#### Editor Store (`src/store/editorStore.js`)
```javascript
import { useEditorStore } from './store/editorStore'

function Editor() {
  const { code, setCode, language, setLanguage, theme } = useEditorStore()
  // ...
}
```

#### Project Store (`src/store/projectStore.js`)
```javascript
import { useProjectStore } from './store/projectStore'

function ProjectList() {
  const { projects, setProjects, currentProject } = useProjectStore()
  // ...
}
```

#### Cloud Store (`src/store/cloudStore.js`)
```javascript
import { useCloudStore } from './store/cloudStore'

function SyncStatus() {
  const { isOnline, isSyncing, syncStatus } = useCloudStore()
  // ...
}
```

## API Integration

### Using the API Client

```javascript
import { projectAPI, fileAPI, executionAPI } from '../services/apiClient'

// List projects
const projects = await projectAPI.list()

// Create file
await fileAPI.create(projectId, 'main.bas', 'PRINT "Hello"')

// Run code
const output = await executionAPI.run(projectId, code, 'BASIC')
```

### Adding New API Endpoints

Edit `src/services/apiClient.js` and add to the appropriate export:

```javascript
export const myAPI = {
  myEndpoint: (id) => apiClient.get(`/my-endpoint/${id}`),
  myOtherEndpoint: (data) => apiClient.post('/my-endpoint', data),
}
```

## Local Storage

### IndexedDB with Dexie

Use the storage service for offline persistence:

```javascript
import { projectsDB, filesDB } from '../services/storage'

// Add project
await projectsDB.add({ name: 'My Project', userId: 123 })

// Get all projects for user
const projects = await projectsDB.getAllForUser(userId)

// Update project
await projectsDB.update(id, { name: 'Updated Name' })
```

## Code Execution

### Running Code

```javascript
import { executeCode } from '../services/interpreter'

try {
  const output = await executeCode(code, language, projectId)
  console.log(output)
} catch (error) {
  console.error('Execution failed:', error)
}
```

## Testing

### Running Tests

```bash
# Run all tests
npm run test

# Run specific test file
npm run test -- authStore.test.js

# Run with coverage
npm run test -- --coverage

# Watch mode
npm run test -- --watch
```

### Writing Tests

Use Vitest with React Testing Library:

```javascript
import { describe, it, expect, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import MyComponent from './MyComponent'

describe('MyComponent', () => {
  it('should render', () => {
    render(<MyComponent />)
    expect(screen.getByText('Hello')).toBeInTheDocument()
  })
})
```

## Building & Deployment

### Building for Production

```bash
npm run build
```

This creates an optimized build in the `dist/` directory.

### Deploy to Vercel

```bash
# Install Vercel CLI
npm i -g vercel

# Deploy
vercel
```

### Deploy to Netlify

```bash
# Install Netlify CLI
npm i -g netlify-cli

# Deploy
netlify deploy --prod --dir=dist
```

### Docker Deployment

```bash
# Build Docker image
docker build -t time-warp-web .

# Run container
docker run -p 3000:3000 time-warp-web

# Or use docker-compose
docker-compose up
```

## Environment Variables

Create `.env.local` with these variables:

```
VITE_API_URL=http://localhost:8000/api
VITE_APP_NAME=Time Warp IDE
VITE_APP_VERSION=6.0.0
VITE_ENABLE_ANALYTICS=true
VITE_ENABLE_PWA=true
VITE_AUTO_SAVE_INTERVAL=30000
VITE_SYNC_INTERVAL=5000
```

## Browser Support

- Chrome/Edge (latest 2 versions)
- Firefox (latest 2 versions)
- Safari (latest 2 versions)
- Mobile browsers (iOS Safari 12+, Chrome Android)

## Performance Tips

1. **Code Splitting**: Components are automatically code-split by Vite
2. **Lazy Loading**: Use React.lazy() for large components
3. **Image Optimization**: Compress images before adding
4. **Bundle Analysis**: Run `npm run analyze` to see bundle size

## Debugging

### VS Code Extensions Recommended

- ES7+ React/Redux/React-Native snippets
- ESLint
- Prettier - Code formatter
- Tailwind CSS IntelliSense

### Browser DevTools

1. React DevTools extension
2. Redux DevTools (if applicable)
3. Network tab for API calls
4. Local Storage tab for persistence

## Troubleshooting

### Port 5173 already in use

```bash
# Find process using port
lsof -i :5173

# Kill process
kill -9 <PID>
```

### Dependencies not installing

```bash
# Clear npm cache
npm cache clean --force

# Remove node_modules
rm -rf node_modules package-lock.json

# Reinstall
npm install
```

### Hot module replacement not working

- Restart dev server: `npm run dev`
- Clear browser cache
- Check for console errors

### Build fails

```bash
# Check for type errors
npm run type-check

# Check for lint errors
npm run lint

# Clean and rebuild
npm run clean
npm run build
```

## Contributing

1. Create a new branch: `git checkout -b feature/my-feature`
2. Make changes and commit: `git commit -am 'Add feature'`
3. Push to branch: `git push origin feature/my-feature`
4. Submit a pull request

## License

See LICENSE file in the project root.

## Support

For issues and questions:
- GitHub Issues: https://github.com/Time-Warp-Studio/Time_Warp_Studio/issues
- Documentation: See docs/ directory
- Maintainer: james@honey-badger.org
