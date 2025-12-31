# Time Warp IDE - Web Version (Vite + WebAssembly)

Browser-based IDE for Time Warp using Vite and WebAssembly for near-native performance.

## Quick Start

```bash
cd Platforms/web
npm install
npm run dev      # Development server
npm run build    # Production build
npm run preview  # Preview production build
```

## Project Structure

```
web/
├── src/
│   ├── main.js              # Entry point
│   ├── App.jsx              # Root component
│   ├── components/          # React components
│   │   ├── Editor.jsx       # Code editor
│   │   ├── Console.jsx      # Output console
│   │   ├── FileTree.jsx     # File browser
│   │   └── Navigation.jsx   # Top nav bar
│   ├── pages/               # Page components
│   │   ├── DashboardPage.jsx
│   │   ├── EditorPage.jsx
│   │   └── SettingsPage.jsx
│   ├── services/            # Business logic
│   │   ├── apiClient.js     # Cloud API calls
│   │   ├── interpreter.js   # WASM interpreter
│   │   └── storage.js       # IndexedDB management
│   ├── store/               # Zustand state
│   │   ├── editorStore.js
│   │   ├── projectStore.js
│   │   └── authStore.js
│   ├── styles/              # CSS/Tailwind
│   │   ├── globals.css
│   │   └── theme.css
│   ├── utils/               # Helpers
│   │   ├── formatters.js
│   │   └── validators.js
│   └── wasm/                # WebAssembly modules
│       ├── interpreter/     # Rust interpreter
│       └── build/           # Compiled WASM
├── public/                  # Static assets
├── index.html              # HTML template
├── vite.config.js          # Vite config
├── package.json            # Dependencies
└── README.md               # Documentation
```

## Features

### Core Editor
- Syntax highlighting for BASIC, Logo, PILOT
- Code folding and minimap
- Multi-file editing
- Search and replace
- Undo/redo with history

### Project Management
- Create, edit, delete projects
- File explorer with drag-drop
- Import/export ZIP files
- Project templates
- Recent projects list

### Execution
- WASM-based interpreter
- Real-time code execution
- Console output
- Error highlighting
- Execution timeout handling

### Cloud Integration
- User authentication
- Project cloud sync
- Collaborative editing (via WebSockets)
- Leaderboard and achievements
- Marketplace integration

### UI/UX
- Responsive design (mobile-friendly)
- Dark/light themes
- Keyboard shortcuts
- Tab-based navigation
- Customizable layouts

## Technology Stack

- **Framework**: React 18
- **Build Tool**: Vite
- **State Management**: Zustand
- **Styling**: Tailwind CSS
- **WASM**: Rust (wasm-bindgen)
- **Code Editor**: Monaco Editor / CodeMirror
- **HTTP Client**: Axios
- **Storage**: IndexedDB + LocalStorage
- **Bundler**: esbuild (via Vite)

## Development

### Install Dependencies
```bash
npm install
```

### Start Dev Server
```bash
npm run dev
# Server runs on http://localhost:5173
```

### Build Production
```bash
npm run build
# Output in dist/
```

### Type Checking
```bash
npm run type-check
```

### Linting
```bash
npm run lint
npm run format
```

## WebAssembly Integration

The interpreter is compiled from Rust to WebAssembly for performance:

```bash
# Build WASM (requires Rust)
cd src/wasm/interpreter
wasm-pack build --target web
```

The WASM module provides:
- Tokenization
- Parsing
- Execution
- Error reporting
- Variable management

## Performance

- **Load time**: < 1 second
- **Editor responsiveness**: 60 FPS
- **Syntax highlighting**: Real-time
- **Code execution**: < 100ms overhead (WASM)
- **Memory**: < 50 MB typical usage

## Browser Support

- Chrome/Edge 90+
- Firefox 87+
- Safari 14+
- Mobile browsers (iOS Safari 14+, Chrome Android)

## Deployment

### Netlify
```bash
npm run build
# Deploy dist/ directory
```

### Vercel
```bash
npm run build
# Automatic deployment on push
```

### Docker
```dockerfile
FROM node:18 as build
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run build

FROM nginx:alpine
COPY --from=build /app/dist /usr/share/nginx/html
EXPOSE 80
```

## Environment Variables

```env
VITE_API_BASE_URL=https://api.timewarp.local:8000/api/v1
VITE_WS_URL=wss://api.timewarp.local:8000
VITE_ENV=production
```

## Testing

```bash
npm run test              # Run tests
npm run test:watch       # Watch mode
npm run test:coverage    # Coverage report
```

## Troubleshooting

### WASM not loading
- Check browser console for 404 errors
- Ensure WASM files are in public/
- Check MIME types (.wasm → application/wasm)

### Slow performance
- Check DevTools Performance tab
- Reduce syntax highlighting complexity
- Use production build
- Enable code splitting

### IndexedDB quota exceeded
- Clear old projects/cache
- Increase quota request
- Use cloud storage instead

## API Reference

See [API Documentation](../../docs/reference/api.md)

## Contributing

See main project [CONTRIBUTING.md](../../CONTRIBUTING.md)

## License

Same as main project (Time Warp IDE)

## Roadmap

### v1.0
- [x] Basic editor
- [x] WASM interpreter
- [x] Cloud sync
- [ ] WebSocket collaboration
- [ ] PWA features

### v1.1
- [ ] Turtle graphics viewport
- [ ] Variable inspector
- [ ] Debugger with breakpoints
- [ ] Performance profiler

### v1.2
- [ ] Live preview (for graphics)
- [ ] Package manager
- [ ] Extension API
- [ ] Theme marketplace
