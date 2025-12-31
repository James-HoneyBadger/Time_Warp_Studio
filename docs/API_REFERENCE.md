# Time Warp Studio - Complete API Reference

## Table of Contents

1. [Authentication](#authentication)
2. [Projects & Sessions](#projects--sessions)
3. [Code Execution](#code-execution)
4. [Collaboration](#collaboration)
5. [Graphics & Rendering](#graphics--rendering)
6. [File Management](#file-management)
7. [User Management](#user-management)
8. [Debugging](#debugging)
9. [Analytics](#analytics)
10. [Admin](#admin)

---

## Authentication

### Register User

```http
POST /api/auth/register
Content-Type: application/json

{
  "username": "john_doe",
  "email": "john@example.com",
  "password": "SecurePassword123!",
  "acceptTerms": true
}
```

**Response (201 Created)**:
```json
{
  "id": "usr_123abc",
  "username": "john_doe",
  "email": "john@example.com",
  "emailVerified": false,
  "createdAt": "2024-01-15T10:30:00Z"
}
```

### Login

```http
POST /api/auth/login
Content-Type: application/json

{
  "email": "john@example.com",
  "password": "SecurePassword123!"
}
```

**Response (200 OK)**:
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "refreshToken": "rt_xyz789",
  "expiresIn": 3600,
  "user": {
    "id": "usr_123abc",
    "username": "john_doe",
    "email": "john@example.com"
  }
}
```

### Refresh Token

```http
POST /api/auth/refresh
Content-Type: application/json

{
  "refreshToken": "rt_xyz789"
}
```

### Logout

```http
POST /api/auth/logout
Authorization: Bearer <token>
```

### Verify Email

```http
POST /api/auth/verify-email
Content-Type: application/json

{
  "code": "123456"
}
```

---

## Projects & Sessions

### Create Project

```http
POST /api/projects
Authorization: Bearer <token>
Content-Type: application/json

{
  "name": "My First Program",
  "description": "Learning BASIC and Logo",
  "language": "basic",
  "isPublic": false,
  "tags": ["learning", "basic"]
}
```

**Response (201 Created)**:
```json
{
  "id": "proj_abc123",
  "name": "My First Program",
  "description": "Learning BASIC and Logo",
  "language": "basic",
  "isPublic": false,
  "owner": "usr_123abc",
  "collaborators": [],
  "createdAt": "2024-01-15T10:30:00Z",
  "updatedAt": "2024-01-15T10:30:00Z",
  "stats": {
    "executions": 0,
    "views": 0,
    "forks": 0
  }
}
```

### Get Project

```http
GET /api/projects/:projectId
Authorization: Bearer <token>
```

### List Projects

```http
GET /api/projects?limit=20&offset=0&sort=updatedAt&order=desc
Authorization: Bearer <token>
```

**Query Parameters**:
- `limit`: Number of projects to return (default: 20, max: 100)
- `offset`: Starting position (default: 0)
- `sort`: Sort field (name, createdAt, updatedAt, views)
- `order`: Sort order (asc, desc)
- `language`: Filter by language (basic, logo, pascal, etc.)
- `search`: Search in name and description

### Update Project

```http
PATCH /api/projects/:projectId
Authorization: Bearer <token>
Content-Type: application/json

{
  "name": "Updated Name",
  "description": "New description",
  "isPublic": true,
  "tags": ["updated", "public"]
}
```

### Delete Project

```http
DELETE /api/projects/:projectId
Authorization: Bearer <token>
```

### Fork Project

```http
POST /api/projects/:projectId/fork
Authorization: Bearer <token>
Content-Type: application/json

{
  "name": "My Fork",
  "isPublic": false
}
```

### Create Session

```http
POST /api/projects/:projectId/sessions
Authorization: Bearer <token>
Content-Type: application/json

{
  "title": "Working Session",
  "code": "PRINT \"Hello, World!\"",
  "language": "basic"
}
```

### Get Session

```http
GET /api/projects/:projectId/sessions/:sessionId
Authorization: Bearer <token>
```

### Update Session

```http
PATCH /api/projects/:projectId/sessions/:sessionId
Authorization: Bearer <token>
Content-Type: application/json

{
  "code": "PRINT \"Updated code\"",
  "title": "Updated Title"
}
```

---

## Code Execution

### Execute Code

```http
POST /api/execute
Authorization: Bearer <token>
Content-Type: application/json

{
  "language": "basic",
  "code": "PRINT \"Hello, World!\"\nFOR i = 1 TO 5\n  PRINT i\nNEXT i",
  "timeout": 5000,
  "debugMode": false,
  "breakpoints": [2, 3]
}
```

**Response (200 OK)**:
```json
{
  "success": true,
  "output": "Hello, World!\n1\n2\n3\n4\n5\n",
  "graphics": [
    {
      "type": "TURTLE_MOVE",
      "x": 50,
      "y": 0,
      "penDown": true
    }
  ],
  "stats": {
    "executionTime": 125,
    "memoryUsed": 2048,
    "outputSize": 256,
    "executionMode": "wasm"
  },
  "debugInfo": {
    "breakpointHit": false,
    "variables": {}
  }
}
```

### Execute with Debugging

```http
POST /api/execute/debug
Authorization: Bearer <token>
Content-Type: application/json

{
  "language": "basic",
  "code": "x = 10\ny = 20\nz = x + y",
  "breakpoints": [2],
  "watches": ["x", "y", "z"]
}
```

**Response (200 OK)**:
```json
{
  "success": true,
  "debugInfo": {
    "breakpointHit": true,
    "currentLine": 2,
    "variables": {
      "x": 10
    },
    "watches": {
      "x": 10,
      "y": null,
      "z": null
    },
    "callStack": [
      {
        "function": "main",
        "line": 2
      }
    ]
  },
  "canContinue": true
}
```

### Step Through Code

```http
POST /api/execute/debug/:debugSessionId/step
Authorization: Bearer <token>
Content-Type: application/json

{
  "stepType": "into"
}
```

**Step Types**:
- `over`: Step to next line
- `into`: Step into function
- `out`: Step out of function
- `continue`: Continue until next breakpoint

### Get Execution History

```http
GET /api/projects/:projectId/executions?limit=50
Authorization: Bearer <token>
```

---

## Collaboration

### Invite Collaborator

```http
POST /api/projects/:projectId/collaborators
Authorization: Bearer <token>
Content-Type: application/json

{
  "email": "collaborator@example.com",
  "role": "editor"
}
```

**Roles**:
- `viewer`: Can view only
- `editor`: Can edit code
- `admin`: Full access

### Share Project

```http
POST /api/projects/:projectId/share
Authorization: Bearer <token>
Content-Type: application/json

{
  "expiresAt": "2024-12-31T23:59:59Z",
  "password": "optional_password",
  "maxViews": 100
}
```

**Response (201 Created)**:
```json
{
  "shareLink": "https://timewarp.io/share/shr_abc123",
  "expiresAt": "2024-12-31T23:59:59Z",
  "maxViews": 100,
  "viewCount": 0
}
```

### Real-time Collaboration (WebSocket)

```javascript
const ws = new WebSocket('wss://api.timewarp.io/ws/projects/:projectId/sessions/:sessionId?token=<token>');

// Messages
ws.onmessage = (event) => {
  const message = JSON.parse(event.data);
  
  if (message.type === 'code-update') {
    // { userId, code, cursorLine, cursorCol }
  } else if (message.type === 'cursor-position') {
    // { userId, line, column, selection }
  } else if (message.type === 'user-joined') {
    // { userId, username }
  } else if (message.type === 'user-left') {
    // { userId }
  }
};

// Broadcast code change
ws.send(JSON.stringify({
  type: 'code-update',
  code: 'new code here',
  cursorLine: 5,
  cursorCol: 10
}));
```

---

## Graphics & Rendering

### Create Canvas

```http
POST /api/canvas
Authorization: Bearer <token>
Content-Type: application/json

{
  "width": 800,
  "height": 600,
  "backgroundColor": "#ffffff"
}
```

### Render Graphics

```http
POST /api/canvas/:canvasId/render
Authorization: Bearer <token>
Content-Type: application/json

{
  "commands": [
    { "type": "TURTLE_MOVE", "x": 100, "y": 100 },
    { "type": "TURTLE_ROTATE", "angle": 90 },
    { "type": "TURTLE_DRAW", "distance": 100 }
  ]
}
```

### Export Canvas

```http
GET /api/canvas/:canvasId/export?format=png
Authorization: Bearer <token>
```

**Formats**: `png`, `jpeg`, `svg`, `webp`

---

## File Management

### Upload File

```http
POST /api/files/upload
Authorization: Bearer <token>
Content-Type: multipart/form-data

file: <binary_data>
```

**Response (201 Created)**:
```json
{
  "id": "file_xyz789",
  "filename": "program.bas",
  "mimeType": "text/plain",
  "size": 1024,
  "uploadedAt": "2024-01-15T10:30:00Z",
  "url": "/files/file_xyz789/download"
}
```

### Download File

```http
GET /api/files/:fileId/download
Authorization: Bearer <token>
```

### List Files

```http
GET /api/files?projectId=proj_abc123&limit=50
Authorization: Bearer <token>
```

### Delete File

```http
DELETE /api/files/:fileId
Authorization: Bearer <token>
```

---

## User Management

### Get Profile

```http
GET /api/users/:userId
Authorization: Bearer <token>
```

### Update Profile

```http
PATCH /api/users/:userId
Authorization: Bearer <token>
Content-Type: application/json

{
  "username": "new_username",
  "avatar": "https://example.com/avatar.png",
  "bio": "Learning to code",
  "theme": "dark"
}
```

### Get User Statistics

```http
GET /api/users/:userId/stats
Authorization: Bearer <token>
```

**Response (200 OK)**:
```json
{
  "totalProjects": 42,
  "totalExecutions": 1523,
  "totalRuntime": 45600000,
  "totalCollaborators": 15,
  "languages": {
    "basic": 25,
    "logo": 12,
    "pascal": 5
  }
}
```

### Change Password

```http
POST /api/users/:userId/password
Authorization: Bearer <token>
Content-Type: application/json

{
  "currentPassword": "OldPassword123!",
  "newPassword": "NewPassword456!"
}
```

### Delete Account

```http
DELETE /api/users/:userId
Authorization: Bearer <token>
Content-Type: application/json

{
  "password": "CurrentPassword123!",
  "reason": "optional_reason"
}
```

---

## Debugging

### List Breakpoints

```http
GET /api/debug/sessions/:sessionId/breakpoints
Authorization: Bearer <token>
```

### Set Breakpoint

```http
POST /api/debug/sessions/:sessionId/breakpoints
Authorization: Bearer <token>
Content-Type: application/json

{
  "line": 5,
  "condition": "x > 10"
}
```

### Add Watch

```http
POST /api/debug/sessions/:sessionId/watches
Authorization: Bearer <token>
Content-Type: application/json

{
  "expression": "x + y * 2"
}
```

### Get Execution Trace

```http
GET /api/debug/sessions/:sessionId/trace
Authorization: Bearer <token>
```

---

## Analytics

### Record Execution

```http
POST /api/analytics/events
Authorization: Bearer <token>
Content-Type: application/json

{
  "eventType": "code_executed",
  "projectId": "proj_abc123",
  "language": "basic",
  "duration": 125,
  "success": true
}
```

### Get Analytics Dashboard

```http
GET /api/analytics/dashboard?period=7d
Authorization: Bearer <token>
```

### Export Analytics Report

```http
GET /api/analytics/export?format=csv&period=30d
Authorization: Bearer <token>
```

---

## Admin

### Get System Status

```http
GET /api/admin/status
Authorization: Bearer <token>
```

### Get System Metrics

```http
GET /api/admin/metrics
Authorization: Bearer <token>
```

### Manage Users (Admin)

```http
GET /api/admin/users?limit=100
Authorization: Bearer <token>
```

### Manage Projects (Admin)

```http
GET /api/admin/projects?limit=100
Authorization: Bearer <token>
```

### Get Server Logs

```http
GET /api/admin/logs?level=error&limit=1000
Authorization: Bearer <token>
```

---

## Error Responses

All error responses follow this format:

```json
{
  "success": false,
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "details": {}
  },
  "requestId": "req_abc123"
}
```

### Common Error Codes

| Code | Status | Description |
|------|--------|-------------|
| INVALID_REQUEST | 400 | Request validation failed |
| UNAUTHORIZED | 401 | Missing or invalid authentication |
| FORBIDDEN | 403 | Insufficient permissions |
| NOT_FOUND | 404 | Resource not found |
| CONFLICT | 409 | Resource conflict (e.g., duplicate) |
| RATE_LIMITED | 429 | Rate limit exceeded |
| SERVER_ERROR | 500 | Internal server error |
| SERVICE_UNAVAILABLE | 503 | Service temporarily unavailable |

---

## Rate Limiting

All endpoints are rate-limited:

- **Free Tier**: 100 requests/hour
- **Pro Tier**: 1000 requests/hour
- **Enterprise**: Unlimited

Rate limit headers:
```
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 999
X-RateLimit-Reset: 1705329600
```

---

## Authentication

All protected endpoints require the `Authorization` header:

```http
Authorization: Bearer <token>
```

Tokens expire after 1 hour. Use the refresh token to get a new token:

```http
POST /api/auth/refresh
```

---

## Pagination

List endpoints support pagination:

- `limit`: Number of items per page (default: 20, max: 100)
- `offset`: Starting position (default: 0)

Response includes:
```json
{
  "data": [...],
  "pagination": {
    "limit": 20,
    "offset": 0,
    "total": 1250,
    "hasMore": true
  }
}
```

---

## WebSocket Events

Real-time events via WebSocket:

| Event | Description |
|-------|-------------|
| `code-update` | Code changed by collaborator |
| `cursor-position` | Cursor moved by collaborator |
| `user-joined` | New user joined session |
| `user-left` | User left session |
| `execution-started` | Code execution started |
| `execution-completed` | Code execution completed |
| `error` | Error occurred |

---

## Webhooks

Set up webhooks to receive events:

```http
POST /api/webhooks
Authorization: Bearer <token>
Content-Type: application/json

{
  "url": "https://example.com/webhook",
  "events": ["execution.completed", "project.updated"],
  "active": true
}
```

Webhook payload:
```json
{
  "id": "evt_abc123",
  "type": "execution.completed",
  "timestamp": "2024-01-15T10:30:00Z",
  "data": {
    "projectId": "proj_abc123",
    "success": true,
    "output": "..."
  }
}
```
