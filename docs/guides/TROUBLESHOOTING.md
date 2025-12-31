# Time Warp IDE Troubleshooting Guide

**Last Updated:** December 31, 2025  
**Status:** Complete Reference

---

## Table of Contents
1. [Backend Issues](#backend-issues)
2. [Frontend Issues](#frontend-issues)
3. [Mobile Issues](#mobile-issues)
4. [Database Issues](#database-issues)
5. [Network & WebSocket Issues](#network--websocket-issues)
6. [Performance Issues](#performance-issues)
7. [Security Issues](#security-issues)
8. [Deployment Issues](#deployment-issues)

---

## Backend Issues

### Service Won't Start

**Symptom:** `uvicorn: error: address already in use`

**Solution:**
```bash
# Find process using port 8000
lsof -i :8000

# Kill process
kill -9 <PID>

# Or use different port
uvicorn main:app --port 8001
```

### Import Errors

**Symptom:** `ModuleNotFoundError: No module named 'fastapi'`

**Solution:**
```bash
# Activate virtual environment
source venv/bin/activate

# Reinstall dependencies
pip install --upgrade pip
pip install -r requirements.txt

# Verify installation
python -c "import fastapi; print(fastapi.__version__)"
```

### Database Connection Failed

**Symptom:** `could not connect to server: Connection refused`

**Solution:**
```bash
# Check PostgreSQL is running
pg_isready -h localhost

# Start PostgreSQL (if stopped)
# Linux:
sudo systemctl start postgresql

# macOS:
brew services start postgresql

# Check connection string
echo $DATABASE_URL

# Test connection directly
psql -h localhost -U postgres -d timewarp_dev

# If connection string is wrong, update .env
# DATABASE_URL=postgresql://user:password@host:port/database
```

### Migration Errors

**Symptom:** `alembic.exc.CommandError: Target database is not current`

**Solution:**
```bash
# Check migration status
python -m alembic current

# Upgrade to latest
python -m alembic upgrade head

# Or downgrade and restart
python -m alembic downgrade base
python -m alembic upgrade head

# Create new migration if needed
python -m alembic revision --autogenerate -m "description"
python -m alembic upgrade head
```

### JWT Token Issues

**Symptom:** `Invalid token` or `Token expired`

**Solution:**
```bash
# Check JWT secret key is set
echo $JWT_SECRET_KEY

# If not set, add to .env
JWT_SECRET_KEY=your-secret-key-here

# Verify token is being sent correctly
# Header: Authorization: Bearer <token>

# Check token expiration
# Default: 1 hour (3600 seconds)
# Adjust in .env: TOKEN_EXPIRE_HOURS=24

# Clear invalid tokens
# Logout and login again to get fresh token
```

### Redis Connection Error

**Symptom:** `ConnectionError: Error 111 connecting to localhost:6379`

**Solution:**
```bash
# Check Redis is running
redis-cli ping

# Start Redis (if stopped)
# Linux:
sudo systemctl start redis-server

# macOS:
brew services start redis

# Check connection string
echo $REDIS_URL

# Test connection
redis-cli -h localhost ping
```

---

## Frontend Issues

### npm Install Fails

**Symptom:** `npm ERR! code ERESOLVE`

**Solution:**
```bash
# Use legacy peer deps
npm install --legacy-peer-deps

# Or use npm version 7+
npm install -g npm@latest

# Clear cache and retry
npm cache clean --force
npm install
```

### Build Fails

**Symptom:** `vite build error`

**Solution:**
```bash
# Check Node.js version
node --version  # Should be 18+

# Clear build artifacts
rm -rf dist/ .vite/

# Rebuild
npm run build

# If still fails, check for errors
npm run build -- --debug
```

### WebSocket Connection Fails

**Symptom:** `WebSocket is closed`

**Solution:**
```bash
# Check backend is running
curl http://localhost:8000/health

# Check environment variables
# .env.local should have:
VITE_API_URL=http://localhost:8000
VITE_WS_URL=ws://localhost:8000

# Check browser console for errors
# Open DevTools (F12) → Console tab

# Clear browser cache and reload
# Hard refresh: Ctrl+Shift+R (Windows/Linux) or Cmd+Shift+R (Mac)

# Check CORS is enabled
# Backend should have: CORS_ORIGINS=http://localhost:3000
```

### State Management Issues

**Symptom:** `Zustand store is empty`

**Solution:**
```bash
# Check store is initialized
// In React component:
const { room } = useStore();
console.log('Room:', room);

// Verify store subscription
useEffect(() => {
  const unsubscribe = useStore.subscribe(
    (state) => console.log('Store updated:', state)
  );
  return unsubscribe;
}, []);

// Check localStorage
// Open DevTools → Application → Local Storage
// Should see timewarp_store data
```

### Memory Leak

**Symptom:** `Memory usage keeps increasing`

**Solution:**
```bash
# Profile with Chrome DevTools
// DevTools → Memory → Heap Snapshot

// Check for unmounted listeners:
useEffect(() => {
  const handler = () => { /* ... */ };
  window.addEventListener('resize', handler);
  
  // Important: Clean up!
  return () => window.removeEventListener('resize', handler);
}, []);

// Check for unmounted subscriptions:
useEffect(() => {
  const unsubscribe = socket.on('event', handler);
  
  // Important: Clean up!
  return () => {
    socket.off('event', handler);
  };
}, [socket]);
```

---

## Mobile Issues

### React Native Setup Issues

**Symptom:** `command not found: react-native`

**Solution:**
```bash
# Install globally
npm install -g @react-native-community/cli

# Or use npx
npx react-native <command>

# Check installation
which react-native
```

### Android Build Fails

**Symptom:** `Could not find android.jar`

**Solution:**
```bash
# Install Android SDK
android update sdk --no-ui --all --filter build-tools-33.0.0
android update sdk --no-ui --all --filter android-33

# Set environment variables
export ANDROID_HOME=~/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

# Clean and rebuild
cd android
./gradlew clean
./gradlew assembleDebug
```

### iOS Build Fails

**Symptom:** `failed to build ios app`

**Solution:**
```bash
# Check Xcode is installed
xcode-select --install

# Clean derived data
rm -rf ~/Library/Developer/Xcode/DerivedData/*

# Install pods
cd ios
pod install --repo-update

# Rebuild
cd ..
npm run ios

# Or use Xcode directly
open ios/TimeWarp.xcworkspace
# Select target → Build
```

### Device Connection Issues

**Symptom:** `Device not found` or `Connection refused`

**Solution:**
```bash
# Android
# Connect device via USB
# Enable USB debugging: Settings → About → Tap Build number 7 times → Developer options → USB debugging

# List connected devices
adb devices

# iOS
# Connect iPhone via USB
# Trust computer on device
# Check in Xcode: Window → Devices and Simulators
```

---

## Database Issues

### Cannot Connect to Database

**Symptom:** `FATAL: Ident authentication failed for user "timewarp"`

**Solution:**
```bash
# Check PostgreSQL is running
systemctl status postgresql

# Verify user exists
sudo -u postgres psql -l

# Create user if missing
sudo -u postgres createuser timewarp
sudo -u postgres psql -c "ALTER USER timewarp WITH PASSWORD 'password';"

# Create database
sudo -u postgres createdb -O timewarp timewarp_prod

# Test connection
psql -h localhost -U timewarp -d timewarp_prod -c "SELECT VERSION();"
```

### Disk Space Full

**Symptom:** `ERROR: could not write block X: No space left on device`

**Solution:**
```bash
# Check disk usage
df -h

# Clean up old logs
sudo journalctl --vacuum=500M

# Check PostgreSQL data size
du -sh /var/lib/postgresql

# Archive old operations
# Create archive table for operations older than 30 days
psql -U timewarp_user << EOF
BEGIN;
CREATE TABLE operations_archive AS 
SELECT * FROM operations 
WHERE created_at < NOW() - INTERVAL '30 days';
DELETE FROM operations WHERE created_at < NOW() - INTERVAL '30 days';
COMMIT;
EOF

# Vacuum database
vacuumdb -U timewarp_user timewarp_prod
```

### Slow Queries

**Symptom:** `Queries taking >1 second`

**Solution:**
```bash
# Enable slow query logging
psql -U timewarp_user << EOF
ALTER SYSTEM SET log_min_duration_statement = 1000;  -- 1 second
SELECT pg_reload_conf();
EOF

# Check slow queries in logs
tail -f /var/log/postgresql/postgresql.log | grep "duration:"

# Analyze query
EXPLAIN ANALYZE SELECT * FROM operations WHERE room_id = '...';

# Create indexes if missing
CREATE INDEX idx_operations_room ON operations(room_id);
CREATE INDEX idx_operations_created ON operations(created_at);
CREATE INDEX idx_messages_room ON messages(room_id);
```

### Data Corruption

**Symptom:** `ERROR: invalid page in block X`

**Solution:**
```bash
# Check database integrity
psql -U timewarp_user timewarp_prod << EOF
REINDEX DATABASE timewarp_prod;
ANALYZE;
EOF

# If still corrupted, restore from backup
pg_restore -U timewarp_user -d timewarp_prod backup.sql

# Prevent corruption in future
# Enable checksums (PostgreSQL 11+)
psql -U timewarp_user timewarp_prod << EOF
ALTER SYSTEM SET data_checksums = on;
EOF
```

---

## Network & WebSocket Issues

### WebSocket Keeps Disconnecting

**Symptom:** `WebSocket connection closed unexpectedly`

**Solution:**
```javascript
// Check reconnection settings
const socket = io('http://localhost:8000', {
  reconnection: true,
  reconnectionDelay: 1000,
  reconnectionDelayMax: 5000,
  reconnectionAttempts: 5
});

// Monitor disconnect events
socket.on('disconnect', (reason) => {
  console.log('Disconnected:', reason);
  // 'io server disconnect' - Server disconnected
  // 'io client disconnect' - Client disconnected
  // 'ping timeout' - No ping response
  // 'transport close' - Transport closed
});

// Check network stability
// Open DevTools → Network tab
// Monitor WebSocket connection
```

### CORS Error

**Symptom:** `Access to XMLHttpRequest blocked by CORS policy`

**Solution:**
```bash
# Check CORS is configured in backend
# .env should have:
CORS_ORIGINS=http://localhost:3000,https://example.com

# Verify headers in response
curl -I http://localhost:8000/api/rooms

# Should include:
# Access-Control-Allow-Origin: http://localhost:3000
# Access-Control-Allow-Credentials: true

# For preflight requests:
# OPTIONS /api/rooms should return 200
curl -X OPTIONS http://localhost:8000/api/rooms -v
```

### Network Timeout

**Symptom:** `Request timeout` or `Connection timeout`

**Solution:**
```bash
# Increase timeout values
// Frontend
const api = axios.create({
  timeout: 30000,  // 30 seconds
  baseURL: process.env.VITE_API_URL
});

// Backend
# .env
REQUEST_TIMEOUT=30
KEEPALIVE_TIMEOUT=65

# Check network latency
ping api.example.com

# Check packet loss
mtr -r -c 100 api.example.com
```

---

## Performance Issues

### High CPU Usage

**Symptom:** `CPU usage >80%`

**Solution:**
```bash
# Identify hot functions
# Backend:
python -m cProfile -s cumtime main.py | head -20

# Frontend:
# DevTools → Performance tab → Record → Analyze

# Check for infinite loops
# Backend logs should show repetitive calls
grep -c "SELECT" logs/access.log

# Reduce query frequency
# Cache results
# Use pagination
# Implement rate limiting
```

### High Memory Usage

**Symptom:** `Memory usage >80%`

**Solution:**
```bash
# Backend memory profiling
pip install memory-profiler
python -m memory_profiler main.py

# Check for memory leaks
# Monitor over time:
ps aux | grep uvicorn | awk '{print $6}'  # RSS column

# Frontend memory
# DevTools → Memory → Heap Snapshot
# Look for detached DOM nodes
# Check for unreleased event listeners

# Database connection pool
# Check: SELECT count(*) FROM pg_stat_activity;
# Limit connections in .env:
DATABASE_POOL_SIZE=10
DATABASE_MAX_OVERFLOW=20
```

### Slow API Responses

**Symptom:** `API requests take >1 second`

**Solution:**
```bash
# Measure response times
curl -w "%{time_total}\n" http://localhost:8000/api/rooms

# Check database query time
# Enable query logging
# .env: SQLALCHEMY_ECHO=True

# Analyze slow queries
EXPLAIN ANALYZE SELECT * FROM operations WHERE room_id = '...';

# Create missing indexes
CREATE INDEX idx_operations_room ON operations(room_id);

# Implement caching
# Cache frequent queries in Redis
# Set TTL: 5 minutes for room data
```

---

## Security Issues

### Unauthorized Access

**Symptom:** `403 Forbidden` on room access

**Solution:**
```bash
# Check user permissions
psql -U timewarp_user << EOF
SELECT room_id, user_id, role FROM collaborators 
WHERE user_id = '550e8400-e29b-41d4-a716-446655440000';
EOF

# Add missing permission
INSERT INTO collaborators (room_id, user_id, role)
VALUES ('650e8400-e29b-41d4-a716-446655440001', 
        '550e8400-e29b-41d4-a716-446655440000', 
        'editor');

# Check token claims
# Decode JWT token at https://jwt.io
# Verify user_id matches request
```

### SQL Injection Attempt

**Symptom:** `error in your SQL syntax`

**Solution:**
```bash
# Never use string concatenation for SQL
# ❌ WRONG:
query = f"SELECT * FROM rooms WHERE name = '{user_input}'"

# ✅ CORRECT:
from sqlalchemy import text
query = text("SELECT * FROM rooms WHERE name = :name")
result = db.execute(query, {"name": user_input})

# Verify all queries use parameterized statements
grep -r "format(" Platforms/backend/app/*.py
grep -r "f\"" Platforms/backend/app/*.py | grep SELECT
```

### XSS Vulnerability

**Symptom:** `Script injection in message content`

**Solution:**
```javascript
// ❌ WRONG:
<div>{message}</div>  // If message contains <script>

// ✅ CORRECT:
// React automatically escapes text content
<div>{message}</div>

// For HTML content, use DOMPurify
import DOMPurify from 'dompurify';
<div dangerouslySetInnerHTML={{ 
  __html: DOMPurify.sanitize(message) 
}} />

// Backend should also sanitize
from bleach import clean
sanitized = clean(user_input, tags=[], strip=True)
```

### Exposed Secrets

**Symptom:** `AWS credentials in .env file`

**Solution:**
```bash
# Never commit secrets to git
# Use .env for development
# Use environment variables for production

# Check for exposed secrets
git log -p -S "aws_secret" --source --all

# Rotate compromised secrets
# If AWS keys exposed:
aws iam delete-access-key --access-key-id AKIA...
aws iam create-access-key

# Use AWS Secrets Manager or similar
# Reference in .env:
# DATABASE_PASSWORD=$(aws secretsmanager get-secret-value --secret-id prod/db/password)
```

---

## Deployment Issues

### Deployment Script Fails

**Symptom:** `deploy.sh: permission denied`

**Solution:**
```bash
# Make script executable
chmod +x deploy.sh

# Run with bash explicitly
bash deploy.sh all prod

# Or debug mode
bash -x deploy.sh all prod
```

### Docker Image Won't Build

**Symptom:** `ERROR: failed to solve with frontend dockerfile.v0`

**Solution:**
```bash
# Check Dockerfile syntax
cat Platforms/backend/Dockerfile | head -20

# Build with verbose output
docker build --progress=plain -t timewarp-api:1.0.0 Platforms/backend

# Check for missing files
ls -la Platforms/backend/requirements.txt

# Build with no cache
docker build --no-cache -t timewarp-api:1.0.0 Platforms/backend
```

### Health Check Fails

**Symptom:** `health check failed`

**Solution:**
```bash
# Manual health check
curl -v http://localhost:8000/health

# Check logs for errors
docker logs <container-id>

# Verify service is listening
netstat -tlnp | grep 8000

# Check environment variables
docker inspect <container-id> | grep Env

# Extend health check timeout
# docker-compose.yml:
healthcheck:
  test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
  interval: 30s
  timeout: 10s
  retries: 3
  start_period: 40s
```

### Kubernetes Pod Won't Start

**Symptom:** `CrashLoopBackOff`

**Solution:**
```bash
# Check pod logs
kubectl logs <pod-name> -n timewarp

# Check pod events
kubectl describe pod <pod-name> -n timewarp

# Check resource limits
kubectl top pod <pod-name> -n timewarp

# Increase resource limits
# Edit deployment:
kubectl edit deployment timewarp-api -n timewarp
# Increase memory: 512Mi → 1Gi
# Reapply: kubectl apply -f deployment.yaml
```

---

## Getting Help

### Debug Information to Collect

```bash
# System info
uname -a
python --version
node --version
docker --version

# Application logs
docker logs <container-id> | tail -100
tail -f logs/timewarp.log

# Database info
psql -U timewarp_user -d timewarp_prod << EOF
SELECT version();
SELECT count(*) FROM pg_stat_activity;
SELECT datname, pg_size_pretty(pg_database_size(datname)) FROM pg_database;
EOF

# Network info
netstat -tlnp | grep -E '8000|3000|5432'
curl -v http://localhost:8000/health

# Environment info
env | grep -E 'DATABASE|REDIS|JWT'
```

### Support Channels

1. **GitHub Issues:** https://github.com/James-HoneyBadger/Time_Warp_Studio/issues
2. **Documentation:** https://github.com/James-HoneyBadger/Time_Warp_Studio/docs
3. **Discord Community:** [invite link]
4. **Email Support:** james@honey-badger.org

---

**Last Updated:** December 31, 2025  
**Version:** 1.0  
**Status:** Complete Reference
