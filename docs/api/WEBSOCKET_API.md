# Time Warp IDE WebSocket API Documentation

**Protocol:** Socket.io 5.9.0  
**Base URL:** `ws://localhost:8000/socket.io`  
**Authentication:** JWT Bearer Token (via query parameter or handshake)

---

## Table of Contents
1. [Connection](#connection)
2. [Authentication](#authentication)
3. [Room Events](#room-events)
4. [Operation Events](#operation-events)
5. [Message Events](#message-events)
6. [Presence Events](#presence-events)
7. [Error Events](#error-events)
8. [Best Practices](#best-practices)

---

## Connection

### Initial Connection

```javascript
import io from 'socket.io-client';

const socket = io('http://localhost:8000', {
  auth: {
    token: accessToken
  },
  reconnection: true,
  reconnectionDelay: 1000,
  reconnectionDelayMax: 5000,
  reconnectionAttempts: 5
});

socket.on('connect', () => {
  console.log('Connected:', socket.id);
});

socket.on('connect_error', (error) => {
  console.error('Connection error:', error);
});

socket.on('disconnect', (reason) => {
  console.log('Disconnected:', reason);
});
```

### Connection Options

```javascript
{
  // Authentication
  auth: {
    token: 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...'
  },
  
  // Reconnection
  reconnection: true,
  reconnectionDelay: 1000,      // Initial delay (ms)
  reconnectionDelayMax: 5000,    // Max delay (ms)
  reconnectionAttempts: 5,       // Max reconnection attempts
  
  // Other options
  transports: ['websocket'],     // Force websocket only
  query: {                         // Query parameters
    'x-api-version': 'v1'
  }
}
```

### Connection States

| State | Event | Description |
|-------|-------|-------------|
| Connecting | `connect_attempt` | Attempting connection |
| Connected | `connect` | Successfully connected |
| Disconnected | `disconnect` | Connection lost |
| Error | `connect_error` | Connection error |
| Reconnecting | `reconnect_attempt` | Retrying connection |

---

## Authentication

### Token-Based Authentication

```javascript
// Option 1: Via auth object
const socket = io('http://localhost:8000', {
  auth: {
    token: accessToken
  }
});

// Option 2: Via query parameter
const socket = io('http://localhost:8000?token=' + accessToken);

// Option 3: Via headers (not supported by Socket.io)
// Use auth object instead
```

### Authentication Events

```javascript
socket.on('authenticated', (data) => {
  console.log('User authenticated:', data.user);
  // {
  //   user: {
  //     id: '550e8400-e29b-41d4-a716-446655440000',
  //     email: 'user@example.com',
  //     name: 'John Doe'
  //   }
  // }
});

socket.on('authentication_error', (error) => {
  console.error('Authentication failed:', error);
  // {
  //   error: 'Invalid token',
  //   message: 'Token verification failed'
  // }
});
```

### Token Refresh

```javascript
socket.on('token_refresh_required', () => {
  // Get new token from server
  fetch('/api/auth/refresh', {
    method: 'POST',
    headers: {
      'Authorization': `Bearer ${refreshToken}`
    }
  })
  .then(res => res.json())
  .then(data => {
    // Emit new token to server
    socket.emit('token_refresh', {
      token: data.access_token
    });
  });
});
```

---

## Room Events

### Join Room

**Client → Server:**
```javascript
socket.emit('join_room', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  user_id: '550e8400-e29b-41d4-a716-446655440000'
}, (response) => {
  console.log('Joined room:', response);
});
```

**Server Response:**
```javascript
// Success (200)
{
  status: 200,
  data: {
    room_id: '650e8400-e29b-41d4-a716-446655440001',
    collaborators: [
      {
        id: '550e8400-e29b-41d4-a716-446655440000',
        name: 'John Doe',
        cursor_position: { line: 5, column: 10 },
        is_online: true
      }
    ],
    code: 'PRINT "Hello, World!"\nEND',
    version: 5
  }
}

// Error (400)
{
  status: 400,
  error: 'Invalid room ID',
  message: 'Room not found'
}
```

**Broadcast to others:**
```javascript
socket.on('user_joined', (data) => {
  console.log('User joined:', data);
  // {
  //   user_id: '550e8400-e29b-41d4-a716-446655440002',
  //   name: 'Jane Doe',
  //   timestamp: '2025-12-31T12:00:00Z'
  // }
});
```

### Leave Room

**Client → Server:**
```javascript
socket.emit('leave_room', {
  room_id: '650e8400-e29b-41d4-a716-446655440001'
}, (response) => {
  console.log('Left room:', response);
});
```

**Broadcast to others:**
```javascript
socket.on('user_left', (data) => {
  console.log('User left:', data);
  // {
  //   user_id: '550e8400-e29b-41d4-a716-446655440002',
  //   name: 'Jane Doe',
  //   timestamp: '2025-12-31T12:05:00Z'
  // }
});
```

---

## Operation Events

### Send Operation

**Client → Server:**
```javascript
socket.emit('operation', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  operation: {
    type: 'insert',
    path: [0, 5],
    value: 'print'
  },
  version: 0,
  timestamp: Date.now()
}, (response) => {
  console.log('Operation saved:', response);
});
```

**Operation Types:**

| Type | Description | Fields |
|------|-------------|--------|
| `insert` | Insert text | `path`, `value` |
| `delete` | Delete text | `path`, `length` |
| `replace` | Replace text | `path`, `value`, `length` |

**Server Response:**
```javascript
// Success (200)
{
  status: 200,
  data: {
    id: '750e8400-e29b-41d4-a716-446655440001',
    version: 1,
    timestamp: '2025-12-31T12:00:00Z'
  }
}

// Conflict (409) - Version mismatch
{
  status: 409,
  error: 'Version conflict',
  current_version: 2,
  operations: [
    // Server's latest operations
  ]
}
```

**Broadcast to others:**
```javascript
socket.on('remote_operation', (data) => {
  console.log('Remote operation:', data);
  // {
  //   operation: {
  //     type: 'insert',
  //     path: [0, 5],
  //     value: 'print'
  //   },
  //   user_id: '550e8400-e29b-41d4-a716-446655440002',
  //   version: 1,
  //   timestamp: '2025-12-31T12:00:00Z'
  // }
});
```

### Request Full Sync

**Client → Server:**
```javascript
socket.emit('request_sync', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  current_version: 0
}, (response) => {
  console.log('Sync received:', response);
});
```

**Server Response:**
```javascript
{
  status: 200,
  data: {
    code: 'PRINT "Hello, World!"\nEND',
    version: 5,
    timestamp: '2025-12-31T12:30:00Z'
  }
}
```

---

## Message Events

### Send Message

**Client → Server:**
```javascript
socket.emit('message', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  content: 'Let\'s add a loop here',
  mentions: ['550e8400-e29b-41d4-a716-446655440002']
}, (response) => {
  console.log('Message sent:', response);
});
```

**Server Response:**
```javascript
{
  status: 200,
  data: {
    id: '850e8400-e29b-41d4-a716-446655440001',
    content: 'Let\'s add a loop here',
    user_id: '550e8400-e29b-41d4-a716-446655440000',
    created_at: '2025-12-31T12:00:00Z'
  }
}
```

**Broadcast to others:**
```javascript
socket.on('new_message', (data) => {
  console.log('New message:', data);
  // {
  //   id: '850e8400-e29b-41d4-a716-446655440001',
  //   content: 'Let\'s add a loop here',
  //   user: {
  //     id: '550e8400-e29b-41d4-a716-446655440000',
  //     name: 'John Doe',
  //     avatar: 'https://example.com/avatars/user.png'
  //   },
  //   mentions: ['550e8400-e29b-41d4-a716-446655440002'],
  //   created_at: '2025-12-31T12:00:00Z'
  // }
});
```

### Edit Message

**Client → Server:**
```javascript
socket.emit('edit_message', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  message_id: '850e8400-e29b-41d4-a716-446655440001',
  content: 'Let\'s add a loop here instead'
}, (response) => {
  console.log('Message edited:', response);
});
```

**Broadcast to others:**
```javascript
socket.on('message_edited', (data) => {
  console.log('Message edited:', data);
  // {
  //   message_id: '850e8400-e29b-41d4-a716-446655440001',
  //   content: 'Let\'s add a loop here instead',
  //   edited_at: '2025-12-31T12:05:00Z'
  // }
});
```

### Delete Message

**Client → Server:**
```javascript
socket.emit('delete_message', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  message_id: '850e8400-e29b-41d4-a716-446655440001'
}, (response) => {
  console.log('Message deleted:', response);
});
```

**Broadcast to others:**
```javascript
socket.on('message_deleted', (data) => {
  console.log('Message deleted:', data);
  // {
  //   message_id: '850e8400-e29b-41d4-a716-446655440001',
  //   deleted_at: '2025-12-31T12:10:00Z'
  // }
});
```

---

## Presence Events

### Update Cursor Position

**Client → Server:**
```javascript
socket.emit('cursor_position', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  position: {
    line: 5,
    column: 10
  }
});
```

**Broadcast to others:**
```javascript
socket.on('remote_cursor', (data) => {
  console.log('Remote cursor:', data);
  // {
  //   user_id: '550e8400-e29b-41d4-a716-446655440002',
  //   name: 'Jane Doe',
  //   color: '#FF6B6B',
  //   position: {
  //     line: 10,
  //     column: 5
  //   }
  // }
});
```

### Update Presence

**Client → Server:**
```javascript
socket.emit('user_presence', {
  room_id: '650e8400-e29b-41d4-a716-446655440001',
  status: 'online',  // 'online', 'away', 'offline'
  activity: 'editing'  // Optional: 'editing', 'reviewing', 'idle'
});
```

**Broadcast to others:**
```javascript
socket.on('presence_changed', (data) => {
  console.log('Presence changed:', data);
  // {
  //   user_id: '550e8400-e29b-41d4-a716-446655440002',
  //   status: 'away',
  //   activity: 'idle'
  // }
});
```

### Get Active Users

**Client → Server:**
```javascript
socket.emit('get_active_users', {
  room_id: '650e8400-e29b-41d4-a716-446655440001'
}, (response) => {
  console.log('Active users:', response);
});
```

**Server Response:**
```javascript
{
  status: 200,
  data: [
    {
      user_id: '550e8400-e29b-41d4-a716-446655440000',
      name: 'John Doe',
      avatar: 'https://example.com/avatars/user.png',
      status: 'online',
      activity: 'editing',
      cursor_position: { line: 5, column: 10 }
    },
    {
      user_id: '550e8400-e29b-41d4-a716-446655440002',
      name: 'Jane Doe',
      avatar: 'https://example.com/avatars/jane.png',
      status: 'away',
      activity: 'idle',
      cursor_position: { line: 10, column: 5 }
    }
  ]
}
```

---

## Error Events

### General Error

```javascript
socket.on('error', (error) => {
  console.error('Socket error:', error);
});
```

### Operation Error

```javascript
socket.on('operation_error', (error) => {
  console.error('Operation error:', error);
  // {
  //   status: 409,
  //   error: 'Version conflict',
  //   current_version: 2,
  //   message: 'Your version is behind. Please sync.'
  // }
});
```

### Room Error

```javascript
socket.on('room_error', (error) => {
  console.error('Room error:', error);
  // {
  //   status: 404,
  //   error: 'Room not found',
  //   message: 'This room no longer exists'
  // }
});
```

### Unauthorized

```javascript
socket.on('unauthorized', (error) => {
  console.error('Unauthorized:', error);
  // {
  //   status: 403,
  //   error: 'Access denied',
  //   message: 'You don\'t have permission to edit this room'
  // }
  
  // Disconnect and redirect to login
  socket.disconnect();
  window.location.href = '/login';
});
```

---

## Best Practices

### 1. Connection Management

```javascript
class SocketManager {
  constructor(token) {
    this.socket = null;
    this.token = token;
    this.reconnectAttempts = 0;
  }
  
  connect() {
    this.socket = io('http://localhost:8000', {
      auth: { token: this.token },
      reconnection: true,
      reconnectionAttempts: 5
    });
    
    this.socket.on('connect', () => {
      this.reconnectAttempts = 0;
      console.log('Connected');
    });
    
    this.socket.on('connect_error', (error) => {
      this.handleConnectionError(error);
    });
  }
  
  handleConnectionError(error) {
    this.reconnectAttempts++;
    if (this.reconnectAttempts >= 5) {
      // Max retries reached
      this.showOfflineMessage();
    }
  }
  
  disconnect() {
    if (this.socket) {
      this.socket.disconnect();
    }
  }
}
```

### 2. Event Listener Cleanup

```javascript
function useSocketListener(socket, event, handler) {
  useEffect(() => {
    socket.on(event, handler);
    
    return () => {
      socket.off(event, handler);
    };
  }, [socket, event, handler]);
}
```

### 3. Acknowledgment Handling

```javascript
socket.emit('operation', operationData, (response) => {
  if (response.status === 200) {
    console.log('Operation saved:', response.data);
  } else if (response.status === 409) {
    // Handle version conflict
    handleVersionConflict(response.data);
  } else {
    // Handle error
    handleError(response.error);
  }
});
```

### 4. Error Handling

```javascript
socket.on('error', (error) => {
  console.error('Socket error:', error);
});

socket.on('connect_error', (error) => {
  console.error('Connection error:', error);
});

socket.on('operation_error', (error) => {
  console.error('Operation error:', error);
  // Retry or show user message
});
```

### 5. Offline Support

```javascript
class OfflineQueue {
  constructor(socket) {
    this.socket = socket;
    this.queue = [];
  }
  
  emit(event, data) {
    if (this.socket.connected) {
      this.socket.emit(event, data);
    } else {
      // Queue for later
      this.queue.push({ event, data });
    }
  }
  
  flush() {
    while (this.queue.length > 0) {
      const { event, data } = this.queue.shift();
      this.socket.emit(event, data);
    }
  }
}
```

---

**Last Updated:** December 31, 2025  
**Protocol Version:** Socket.io 5.9.0  
**Status:** Stable
