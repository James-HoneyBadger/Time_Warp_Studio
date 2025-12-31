# Time Warp IDE REST API Documentation

**API Version:** 1.0.0  
**Base URL:** `http://localhost:8000/api`  
**Authentication:** JWT Bearer Token

---

## Table of Contents
1. [Authentication](#authentication)
2. [Users](#users)
3. [Rooms](#rooms)
4. [Operations](#operations)
5. [Messages](#messages)
6. [Collaborators](#collaborators)
7. [Error Handling](#error-handling)
8. [Rate Limiting](#rate-limiting)

---

## Authentication

### Login
Create a new session and receive JWT token.

```
POST /auth/login
Content-Type: application/json

{
  "email": "user@example.com",
  "password": "securepassword"
}
```

**Response (200 OK):**
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expires_in": 3600,
  "user": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "email": "user@example.com",
    "name": "John Doe"
  }
}
```

### Register
Create a new user account.

```
POST /auth/register
Content-Type: application/json

{
  "email": "newuser@example.com",
  "password": "securepassword",
  "name": "New User"
}
```

**Response (201 Created):**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "email": "newuser@example.com",
  "name": "New User",
  "created_at": "2025-12-31T12:00:00Z"
}
```

### Refresh Token
Get new access token using refresh token.

```
POST /auth/refresh
Authorization: Bearer <refresh_token>
```

**Response (200 OK):**
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expires_in": 3600
}
```

### Logout
Invalidate current session.

```
POST /auth/logout
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "message": "Successfully logged out"
}
```

---

## Users

### Get Current User
Retrieve authenticated user's profile.

```
GET /users/me
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "email": "user@example.com",
  "name": "John Doe",
  "avatar": "https://example.com/avatars/user.png",
  "created_at": "2025-12-31T10:00:00Z",
  "updated_at": "2025-12-31T12:00:00Z"
}
```

### Update User Profile
Update user information.

```
PUT /users/me
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "name": "John Updated",
  "avatar": "https://example.com/avatars/new.png"
}
```

**Response (200 OK):**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "email": "user@example.com",
  "name": "John Updated",
  "avatar": "https://example.com/avatars/new.png",
  "updated_at": "2025-12-31T12:30:00Z"
}
```

### Change Password
Update user password.

```
POST /users/me/change-password
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "old_password": "currentpassword",
  "new_password": "newpassword"
}
```

**Response (200 OK):**
```json
{
  "message": "Password changed successfully"
}
```

---

## Rooms

### Create Room
Create a new collaborative room.

```
POST /rooms
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "name": "My First Project",
  "description": "Learning BASIC programming",
  "language": "basic",
  "is_public": false
}
```

**Response (201 Created):**
```json
{
  "id": "650e8400-e29b-41d4-a716-446655440001",
  "name": "My First Project",
  "description": "Learning BASIC programming",
  "language": "basic",
  "is_public": false,
  "owner_id": "550e8400-e29b-41d4-a716-446655440000",
  "created_at": "2025-12-31T12:00:00Z",
  "collaborators_count": 1
}
```

### Get Rooms
List user's rooms or public rooms.

```
GET /rooms?page=1&limit=20&sort=updated_at&order=desc
Authorization: Bearer <access_token>
```

**Query Parameters:**
- `page` (int, default=1): Page number
- `limit` (int, default=20, max=100): Items per page
- `sort` (string, default=updated_at): Sort by field (name, created_at, updated_at)
- `order` (string, default=desc): Sort order (asc, desc)
- `filter` (string): Filter by name or language

**Response (200 OK):**
```json
{
  "items": [
    {
      "id": "650e8400-e29b-41d4-a716-446655440001",
      "name": "My First Project",
      "description": "Learning BASIC programming",
      "language": "basic",
      "is_public": false,
      "owner_id": "550e8400-e29b-41d4-a716-446655440000",
      "created_at": "2025-12-31T12:00:00Z",
      "updated_at": "2025-12-31T12:30:00Z",
      "collaborators_count": 1
    }
  ],
  "total": 1,
  "page": 1,
  "limit": 20,
  "pages": 1
}
```

### Get Room
Get specific room details.

```
GET /rooms/{room_id}
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "id": "650e8400-e29b-41d4-a716-446655440001",
  "name": "My First Project",
  "description": "Learning BASIC programming",
  "language": "basic",
  "is_public": false,
  "owner_id": "550e8400-e29b-41d4-a716-446655440000",
  "code": "PRINT \"Hello, World!\"\nEND",
  "created_at": "2025-12-31T12:00:00Z",
  "updated_at": "2025-12-31T12:30:00Z",
  "collaborators": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "name": "John Doe",
      "avatar": "https://example.com/avatars/user.png",
      "role": "owner"
    }
  ]
}
```

### Update Room
Update room metadata.

```
PUT /rooms/{room_id}
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "name": "Updated Project Name",
  "description": "Updated description",
  "is_public": true
}
```

**Response (200 OK):**
```json
{
  "id": "650e8400-e29b-41d4-a716-446655440001",
  "name": "Updated Project Name",
  "description": "Updated description",
  "is_public": true,
  "updated_at": "2025-12-31T13:00:00Z"
}
```

### Delete Room
Delete a room (owner only).

```
DELETE /rooms/{room_id}
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "message": "Room deleted successfully"
}
```

---

## Operations

### Save Code
Save operation in room (triggered during collaborative editing).

```
POST /rooms/{room_id}/operations
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "type": "insert",
  "path": [0, 5],
  "value": "print",
  "timestamp": 1704067200000,
  "user_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

**Response (201 Created):**
```json
{
  "id": "750e8400-e29b-41d4-a716-446655440001",
  "operation": {
    "type": "insert",
    "path": [0, 5],
    "value": "print"
  },
  "version": 1,
  "timestamp": "2025-12-31T12:00:00Z"
}
```

### Get Operations
Retrieve operation history.

```
GET /rooms/{room_id}/operations?from_version=0&limit=100
Authorization: Bearer <access_token>
```

**Query Parameters:**
- `from_version` (int, default=0): Start from version
- `limit` (int, default=100, max=1000): Max operations

**Response (200 OK):**
```json
{
  "operations": [
    {
      "id": "750e8400-e29b-41d4-a716-446655440001",
      "operation": {
        "type": "insert",
        "path": [0, 5],
        "value": "print"
      },
      "version": 1,
      "user_id": "550e8400-e29b-41d4-a716-446655440000",
      "timestamp": "2025-12-31T12:00:00Z"
    }
  ],
  "current_version": 1,
  "total": 1
}
```

### Sync Code
Get latest code snapshot.

```
GET /rooms/{room_id}/sync
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "code": "PRINT \"Hello, World!\"\nEND",
  "version": 5,
  "last_updated": "2025-12-31T12:30:00Z"
}
```

---

## Messages

### Send Message
Send chat message to room.

```
POST /rooms/{room_id}/messages
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "content": "Let's add a loop here",
  "mentions": ["550e8400-e29b-41d4-a716-446655440002"]
}
```

**Response (201 Created):**
```json
{
  "id": "850e8400-e29b-41d4-a716-446655440001",
  "content": "Let's add a loop here",
  "user_id": "550e8400-e29b-41d4-a716-446655440000",
  "user": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "name": "John Doe",
    "avatar": "https://example.com/avatars/user.png"
  },
  "mentions": ["550e8400-e29b-41d4-a716-446655440002"],
  "created_at": "2025-12-31T12:00:00Z"
}
```

### Get Messages
Retrieve room messages.

```
GET /rooms/{room_id}/messages?page=1&limit=50
Authorization: Bearer <access_token>
```

**Query Parameters:**
- `page` (int, default=1): Page number
- `limit` (int, default=50, max=100): Messages per page

**Response (200 OK):**
```json
{
  "items": [
    {
      "id": "850e8400-e29b-41d4-a716-446655440001",
      "content": "Let's add a loop here",
      "user_id": "550e8400-e29b-41d4-a716-446655440000",
      "user": {
        "id": "550e8400-e29b-41d4-a716-446655440000",
        "name": "John Doe",
        "avatar": "https://example.com/avatars/user.png"
      },
      "mentions": ["550e8400-e29b-41d4-a716-446655440002"],
      "created_at": "2025-12-31T12:00:00Z",
      "edited_at": null
    }
  ],
  "total": 1,
  "page": 1,
  "limit": 50,
  "pages": 1
}
```

### Edit Message
Update message content.

```
PUT /rooms/{room_id}/messages/{message_id}
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "content": "Let's add a loop here instead"
}
```

**Response (200 OK):**
```json
{
  "id": "850e8400-e29b-41d4-a716-446655440001",
  "content": "Let's add a loop here instead",
  "edited_at": "2025-12-31T12:05:00Z"
}
```

### Delete Message
Delete message.

```
DELETE /rooms/{room_id}/messages/{message_id}
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "message": "Message deleted successfully"
}
```

---

## Collaborators

### Add Collaborator
Invite user to room.

```
POST /rooms/{room_id}/collaborators
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "user_id": "550e8400-e29b-41d4-a716-446655440002",
  "role": "editor"
}
```

**Response (201 Created):**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440002",
  "name": "Jane Doe",
  "avatar": "https://example.com/avatars/jane.png",
  "role": "editor",
  "joined_at": "2025-12-31T12:00:00Z"
}
```

### List Collaborators
Get all collaborators in room.

```
GET /rooms/{room_id}/collaborators
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
[
  {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "name": "John Doe",
    "avatar": "https://example.com/avatars/user.png",
    "role": "owner",
    "joined_at": "2025-12-31T12:00:00Z",
    "cursor_position": { "line": 5, "column": 10 },
    "is_online": true
  },
  {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "name": "Jane Doe",
    "avatar": "https://example.com/avatars/jane.png",
    "role": "editor",
    "joined_at": "2025-12-31T12:30:00Z",
    "cursor_position": { "line": 10, "column": 5 },
    "is_online": true
  }
]
```

### Update Collaborator Role
Change collaborator permission.

```
PUT /rooms/{room_id}/collaborators/{user_id}
Authorization: Bearer <access_token>
Content-Type: application/json

{
  "role": "viewer"
}
```

**Response (200 OK):**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440002",
  "name": "Jane Doe",
  "role": "viewer",
  "updated_at": "2025-12-31T12:45:00Z"
}
```

### Remove Collaborator
Remove user from room.

```
DELETE /rooms/{room_id}/collaborators/{user_id}
Authorization: Bearer <access_token>
```

**Response (200 OK):**
```json
{
  "message": "Collaborator removed successfully"
}
```

---

## Error Handling

### Standard Error Response

```json
{
  "error": "Unauthorized",
  "status_code": 401,
  "detail": "Invalid or expired token",
  "request_id": "req-550e8400-e29b-41d4-a716-446655440000"
}
```

### Common Status Codes

| Code | Description |
|------|-------------|
| 200 | Success |
| 201 | Created |
| 204 | No Content |
| 400 | Bad Request |
| 401 | Unauthorized |
| 403 | Forbidden |
| 404 | Not Found |
| 409 | Conflict |
| 422 | Unprocessable Entity |
| 429 | Too Many Requests |
| 500 | Internal Server Error |

### Error Examples

**401 Unauthorized:**
```json
{
  "error": "Unauthorized",
  "status_code": 401,
  "detail": "Missing or invalid authentication token"
}
```

**404 Not Found:**
```json
{
  "error": "Not Found",
  "status_code": 404,
  "detail": "Room not found"
}
```

**422 Validation Error:**
```json
{
  "error": "Validation Error",
  "status_code": 422,
  "detail": [
    {
      "field": "email",
      "message": "Invalid email format"
    }
  ]
}
```

---

## Rate Limiting

### Rate Limit Headers

Every response includes rate limit information:

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 75
X-RateLimit-Reset: 1704067260
```

### Rate Limits by Endpoint

| Endpoint | Limit | Window |
|----------|-------|--------|
| Login | 5 requests | 15 minutes |
| Register | 3 requests | 1 hour |
| General API | 100 requests | 1 minute |
| WebSocket | 1000 events | 1 minute |

### Rate Limit Response

```json
{
  "error": "Too Many Requests",
  "status_code": 429,
  "detail": "Rate limit exceeded. Try again in 60 seconds"
}
```

---

## Examples

### Complete Login & Create Room Flow

```bash
# 1. Login
curl -X POST http://localhost:8000/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"user@example.com","password":"password"}'

# Response includes access_token

# 2. Create Room
curl -X POST http://localhost:8000/api/rooms \
  -H "Authorization: Bearer <access_token>" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "My Project",
    "language": "basic",
    "is_public": false
  }'

# 3. Get Room
curl -X GET http://localhost:8000/api/rooms/{room_id} \
  -H "Authorization: Bearer <access_token>"

# 4. Save Code Operation
curl -X POST http://localhost:8000/api/rooms/{room_id}/operations \
  -H "Authorization: Bearer <access_token>" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "insert",
    "path": [0, 5],
    "value": "print",
    "timestamp": 1704067200000
  }'
```

---

**Last Updated:** December 31, 2025  
**API Version:** 1.0.0  
**Status:** Stable
