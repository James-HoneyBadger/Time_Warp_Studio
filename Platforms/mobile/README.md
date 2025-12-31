# Time Warp IDE - Mobile App (React Native)

This directory contains the React Native mobile application for Time Warp IDE, providing a lightweight IDE experience on iOS and Android devices.

## Project Structure

```
mobile/
├── package.json              # Dependencies and scripts
├── app.json                  # App configuration (Expo)
├── index.js                  # App entry point
├── src/
│   ├── screens/             # Screen components
│   │   ├── EditorScreen.js   # Code editor
│   │   ├── ConsoleScreen.js  # Output console
│   │   ├── FilesScreen.js    # Project files
│   │   ├── CloudScreen.js    # Cloud sync
│   │   └── SettingsScreen.js # App settings
│   ├── components/          # Reusable components
│   │   ├── CodeEditor.js     # Editor component
│   │   ├── Console.js        # Console component
│   │   ├── FileBrowser.js    # File browser
│   │   └── CloudSync.js      # Cloud sync UI
│   ├── services/            # Business logic
│   │   ├── api.js            # Cloud API client
│   │   ├── interpreter.js    # Language interpreter
│   │   └── storage.js        # Local storage manager
│   ├── styles/              # Global styles
│   │   └── theme.js          # Theme definitions
│   ├── utils/               # Utility functions
│   │   ├── logger.js         # Logging
│   │   └── validators.js     # Input validation
│   └── store/               # State management (Redux)
│       ├── actions.js        # Redux actions
│       ├── reducers.js       # Redux reducers
│       └── store.js          # Store configuration
├── __tests__/               # Test files
│   ├── components/
│   ├── screens/
│   └── services/
└── README.md               # Documentation
```

## Features

### Phase 4.3A: Core Editor
- Lightweight code editor with syntax highlighting
- Support for BASIC, Logo, PILOT languages
- Multi-file project support
- Auto-save to local storage
- Offline mode support

### Phase 4.3B: Project Management
- Create and manage projects locally
- File browser with add/delete/rename
- Project export/import (ZIP)
- Cloud project synchronization
- Version history (local)

### Phase 4.3C: Execution & Console
- In-app interpreter execution
- Output console with ANSI color support
- Error highlighting
- Input prompts with keyboard
- Execution timeout handling

### Phase 4.3D: Cloud Integration
- Cloud project sync with offline queuing
- Authentication (login/register)
- Cloud backup and restore
- Collaborative features (when online)
- Leaderboard integration

### Phase 4.3E: UI & UX
- Material Design 3 components
- Dark/light theme support
- Responsive layout for phones/tablets
- Touch-optimized editor
- Bottom navigation for screens

## Installation

```bash
# Install dependencies
npm install

# Start Expo development server
npx expo start

# Run on iOS simulator
npx expo run:ios

# Run on Android emulator
npx expo run:android

# Build APK for distribution
eas build --platform android --local
```

## Development

### Using Expo
```bash
# Start in development mode
npx expo start

# Scan QR code with Expo app on phone to test
# Changes hot-reload automatically
```

### Testing
```bash
# Run unit tests
npm test

# Run integration tests
npm run test:integration

# Generate coverage report
npm run test:coverage
```

## API Integration

The mobile app connects to the Time Warp Cloud API:

```javascript
// Example: Login and get projects
const response = await api.login({
  email: 'user@example.com',
  password: 'password123'
});

const projects = await api.getProjects(response.access_token);
```

## Storage

Local storage uses AsyncStorage for persisting:
- User authentication tokens
- Project files and metadata
- Editor preferences
- Sync queue for offline changes
- Execution history

## Performance

- App size target: < 30 MB APK
- Startup time target: < 2 seconds
- Editor responsiveness: 60 FPS
- Memory usage: < 150 MB on device
- Battery usage: Optimized with background timers

## Troubleshooting

### App won't start
```bash
# Clear cache and reinstall
npm run clean
npm install
npx expo start --clear
```

### Sync issues
- Check network connectivity
- Verify cloud server is running
- Check authentication tokens
- Review sync queue status

### Performance issues
- Reduce syntax highlighting frequency
- Limit file size to 1 MB
- Use production builds for testing
- Profile with React Native Debugger

## Future Enhancements

- Turtle graphics viewport
- Collaborative editing (CRDTs)
- Voice input for code
- AR-based visualization
- Hardware device connectivity (IoT)
- Push notifications for sync

## Contributing

See main project CONTRIBUTING.md for guidelines.

## License

Same as main Time Warp IDE project.
