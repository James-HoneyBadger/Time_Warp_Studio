# Time Warp IDE Deployment Guide

**Version:** 1.0  
**Last Updated:** December 31, 2025  
**Status:** Production-Ready

---

## Table of Contents
1. [Quick Start](#quick-start)
2. [Prerequisites](#prerequisites)
3. [Development Environment](#development-environment)
4. [Staging Deployment](#staging-deployment)
5. [Production Deployment](#production-deployment)
6. [Docker Deployment](#docker-deployment)
7. [Kubernetes Deployment](#kubernetes-deployment)
8. [Cloud Provider Setup](#cloud-provider-setup)
9. [Monitoring & Logging](#monitoring--logging)
10. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Automated Deployment (Recommended)

```bash
# Navigate to project root
cd /home/james/Time_Warp_Studio

# Deploy everything to production
./deploy.sh all prod

# Or deploy specific component
./deploy.sh backend staging
./deploy.sh frontend dev
./deploy.sh mobile prod
```

### Manual Quick Start

```bash
# 1. Backend Setup
cd Platforms/backend
pip install -r requirements.txt
python -m uvicorn main:app --reload

# 2. Frontend Setup (in new terminal)
cd Platforms/web
npm install
npm run dev

# 3. Mobile Setup (in new terminal)
cd Platforms/mobile
npm install
npm run android  # or npm run ios
```

---

## Prerequisites

### System Requirements

**All Platforms:**
- OS: Linux (Ubuntu 20.04+), macOS 11+, Windows 10+
- Python 3.10+
- Node.js 18+
- Docker 20.10+
- Git 2.30+

**Backend Specific:**
- PostgreSQL 13+
- Redis 6.0+
- 2GB RAM minimum
- 10GB disk space

**Frontend Specific:**
- Modern browser (Chrome, Firefox, Safari, Edge)
- 4GB RAM for build process

**Mobile Specific (iOS):**
- macOS 11+
- Xcode 13+
- iOS 12+ target

**Mobile Specific (Android):**
- Android Studio 4.2+
- Java 11+
- Android API 24+

### Required Tools

```bash
# Check Python
python3 --version  # Should be 3.10+

# Check Node.js
node --version     # Should be 18+

# Check Docker
docker --version

# Check Git
git --version

# Install pip packages (backend)
pip install -r Platforms/backend/requirements.txt

# Install npm packages (frontend & mobile)
npm install -g @vue/cli @react-native-community/cli
```

---

## Development Environment

### Setup Backend Development

```bash
cd Platforms/backend

# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Create environment file
cat > .env << EOF
DATABASE_URL=postgresql://user:password@localhost/timewarp_dev
REDIS_URL=redis://localhost:6379
JWT_SECRET_KEY=dev-secret-key-change-in-production
ENVIRONMENT=development
DEBUG=True
ALLOWED_HOSTS=localhost,127.0.0.1
CORS_ORIGINS=http://localhost:3000,http://localhost:8081
EOF

# Initialize database
python -m alembic upgrade head

# Run tests
python -m pytest tests/ -v

# Start development server
python -m uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

### Setup Frontend Development

```bash
cd Platforms/web

# Install dependencies
npm install

# Create environment file
cat > .env.local << EOF
VITE_API_URL=http://localhost:8000
VITE_WS_URL=ws://localhost:8000
VITE_ENVIRONMENT=development
EOF

# Run development server
npm run dev

# Run tests
npm test

# Build for production
npm run build
```

### Setup Mobile Development

```bash
cd Platforms/mobile

# Install dependencies
npm install

# Create environment file
cat > .env << EOF
API_URL=http://localhost:8000
WS_URL=ws://localhost:8000
ENVIRONMENT=development
EOF

# Android development
npm run android

# iOS development (macOS only)
npm run ios

# Run tests
npm test
```

---

## Staging Deployment

### Deploy to Staging Environment

```bash
# Deploy all components
./deploy.sh all staging

# Or deploy individually
./deploy.sh backend staging
./deploy.sh frontend staging
./deploy.sh mobile staging
```

### Staging Environment Configuration

**Backend (.env.staging):**
```bash
DATABASE_URL=postgresql://user:password@staging-db.example.com/timewarp_staging
REDIS_URL=redis://staging-redis.example.com:6379
JWT_SECRET_KEY=<secure-staging-key>
ENVIRONMENT=staging
DEBUG=False
ALLOWED_HOSTS=staging-api.example.com
CORS_ORIGINS=https://staging-app.example.com
```

**Frontend (.env.staging):**
```bash
VITE_API_URL=https://staging-api.example.com
VITE_WS_URL=wss://staging-api.example.com
VITE_ENVIRONMENT=staging
```

### Staging Health Checks

```bash
# Check API health
curl https://staging-api.example.com/health

# Check WebSocket connection
npm run test:websocket

# Run smoke tests
npm run test:staging

# Check database connectivity
python manage.py dbshell

# Verify backend services
curl https://staging-api.example.com/api/health/status
```

---

## Production Deployment

### Pre-Deployment Checklist

```bash
# 1. Verify all tests pass
./deploy.sh backend prod --test-only
./deploy.sh frontend prod --test-only

# 2. Run security audit
bandit -r Platforms/backend/
npm run lint

# 3. Check performance metrics
python -m pytest Platforms/backend/tests/test_load_performance.py -v

# 4. Backup database
pg_dump -h prod-db.example.com -U postgres timewarp_prod > backup_$(date +%Y%m%d_%H%M%S).sql

# 5. Create deployment branch
git checkout -b deploy/prod-$(date +%Y%m%d)
```

### Production Deployment

```bash
# Full production deployment
./deploy.sh all prod

# Or step by step
./deploy.sh backend prod
./deploy.sh frontend prod
./deploy.sh mobile prod
```

### Production Environment Configuration

**Backend (.env.production):**
```bash
DATABASE_URL=postgresql://timewarp_user:$(aws secretsmanager get-secret-value --secret-id prod/db/password --query SecretString --output text)@prod-db.example.com:5432/timewarp_prod
REDIS_URL=redis://:$(aws secretsmanager get-secret-value --secret-id prod/redis/password --query SecretString --output text)@prod-redis.example.com:6379
JWT_SECRET_KEY=$(aws secretsmanager get-secret-value --secret-id prod/jwt/secret --query SecretString --output text)
ENVIRONMENT=production
DEBUG=False
ALLOWED_HOSTS=api.timewarp.example.com
CORS_ORIGINS=https://timewarp.example.com
LOG_LEVEL=INFO
SENTRY_DSN=$(aws secretsmanager get-secret-value --secret-id prod/sentry/dsn --query SecretString --output text)
```

**Frontend (.env.production):**
```bash
VITE_API_URL=https://api.timewarp.example.com
VITE_WS_URL=wss://api.timewarp.example.com
VITE_ENVIRONMENT=production
VITE_SENTRY_DSN=$(aws secretsmanager get-secret-value --secret-id prod/sentry/dsn --query SecretString --output text)
```

### Post-Deployment Validation

```bash
# Check API health
curl https://api.timewarp.example.com/health

# Verify WebSocket
npm run test:websocket:prod

# Check database
psql -h prod-db.example.com -U timewarp_user -d timewarp_prod -c "SELECT COUNT(*) FROM users;"

# Monitor error rates
# Check Sentry dashboard at sentry.io/organizations/timewarp

# Check performance metrics
# CloudWatch dashboard: https://console.aws.amazon.com/cloudwatch
```

---

## Docker Deployment

### Build Docker Images

```bash
# Backend image
cd Platforms/backend
docker build -t timewarp-api:1.0.0 .
docker tag timewarp-api:1.0.0 timewarp-api:latest

# Frontend image
cd Platforms/web
docker build -t timewarp-web:1.0.0 .
docker tag timewarp-web:1.0.0 timewarp-web:latest

# Push to registry
docker push registry.example.com/timewarp-api:1.0.0
docker push registry.example.com/timewarp-web:1.0.0
```

### Docker Compose (Development)

```bash
cd Platforms
docker-compose up -d

# Check status
docker-compose ps

# View logs
docker-compose logs -f api
docker-compose logs -f web

# Stop services
docker-compose down
```

### Docker Compose Configuration

```yaml
version: '3.8'

services:
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: timewarp_dev
      POSTGRES_PASSWORD: dev_password
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7
    ports:
      - "6379:6379"

  api:
    build: ./backend
    environment:
      DATABASE_URL: postgresql://postgres:dev_password@postgres:5432/timewarp_dev
      REDIS_URL: redis://redis:6379
    ports:
      - "8000:8000"
    depends_on:
      - postgres
      - redis

  web:
    build: ./web
    ports:
      - "3000:3000"
    depends_on:
      - api

volumes:
  postgres_data:
```

---

## Kubernetes Deployment

### Prerequisites

```bash
# Install kubectl
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

# Install Helm (optional)
curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash

# Configure kubectl
kubectl config use-context timewarp-prod-cluster
```

### Kubernetes Manifests

**Backend Deployment (backend-deployment.yaml):**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: timewarp-api
  namespace: timewarp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: timewarp-api
  template:
    metadata:
      labels:
        app: timewarp-api
    spec:
      containers:
      - name: api
        image: registry.example.com/timewarp-api:1.0.0
        ports:
        - containerPort: 8000
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: timewarp-secrets
              key: database-url
        - name: REDIS_URL
          valueFrom:
            secretKeyRef:
              name: timewarp-secrets
              key: redis-url
        resources:
          requests:
            cpu: 500m
            memory: 512Mi
          limits:
            cpu: 1000m
            memory: 1Gi
        livenessProbe:
          httpGet:
            path: /health
            port: 8000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 8000
          initialDelaySeconds: 10
          periodSeconds: 5
```

**Service (backend-service.yaml):**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: timewarp-api
  namespace: timewarp
spec:
  type: LoadBalancer
  selector:
    app: timewarp-api
  ports:
  - protocol: TCP
    port: 80
    targetPort: 8000
```

### Deploy to Kubernetes

```bash
# Create namespace
kubectl create namespace timewarp

# Create secrets
kubectl create secret generic timewarp-secrets \
  --from-literal=database-url="postgresql://..." \
  --from-literal=redis-url="redis://..." \
  -n timewarp

# Deploy backend
kubectl apply -f kubernetes/backend-deployment.yaml
kubectl apply -f kubernetes/backend-service.yaml

# Check deployment status
kubectl get deployments -n timewarp
kubectl get pods -n timewarp
kubectl get svc -n timewarp

# View logs
kubectl logs -n timewarp deployment/timewarp-api

# Scale deployment
kubectl scale deployment timewarp-api --replicas=5 -n timewarp
```

---

## Cloud Provider Setup

### AWS Deployment

```bash
# 1. Create RDS PostgreSQL database
aws rds create-db-instance \
  --db-instance-identifier timewarp-prod \
  --db-instance-class db.t3.micro \
  --engine postgres \
  --master-username admin \
  --master-user-password <strong-password>

# 2. Create ElastiCache Redis
aws elasticache create-cache-cluster \
  --cache-cluster-id timewarp-redis \
  --cache-node-type cache.t3.micro \
  --engine redis

# 3. Push Docker image to ECR
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 123456789.dkr.ecr.us-east-1.amazonaws.com
docker tag timewarp-api:1.0.0 123456789.dkr.ecr.us-east-1.amazonaws.com/timewarp-api:1.0.0
docker push 123456789.dkr.ecr.us-east-1.amazonaws.com/timewarp-api:1.0.0

# 4. Deploy to ECS
aws ecs create-service \
  --cluster timewarp-prod \
  --service-name timewarp-api \
  --task-definition timewarp-api:1

# 5. Upload frontend to S3
aws s3 sync Platforms/web/dist s3://timewarp-web-prod/ --delete

# 6. Invalidate CloudFront
aws cloudfront create-invalidation \
  --distribution-id E1234ABCD \
  --paths "/*"
```

### Google Cloud Deployment

```bash
# 1. Create Cloud SQL PostgreSQL
gcloud sql instances create timewarp-prod \
  --database-version=POSTGRES_15 \
  --tier=db-f1-micro \
  --region=us-central1

# 2. Create Cloud Memorystore Redis
gcloud redis instances create timewarp-redis \
  --size=1 \
  --region=us-central1

# 3. Build and push to Container Registry
gcloud builds submit Platforms/backend --tag gcr.io/PROJECT-ID/timewarp-api

# 4. Deploy to Cloud Run
gcloud run deploy timewarp-api \
  --image gcr.io/PROJECT-ID/timewarp-api:latest \
  --platform managed \
  --region us-central1 \
  --set-env-vars DATABASE_URL=cloudsql://...

# 5. Deploy frontend to Cloud Storage
gsutil -m cp -r Platforms/web/dist/* gs://timewarp-web-prod/
```

---

## Monitoring & Logging

### Application Monitoring

```bash
# Backend health check
curl -X GET http://localhost:8000/health

# Database health
psql -h localhost -U timewarp_user -d timewarp_prod \
  -c "SELECT VERSION(); SELECT COUNT(*) FROM users;"

# Redis health
redis-cli -h localhost ping

# View logs
# Backend
docker logs <container-id>

# Frontend (browser console)
# Open DevTools (F12) and check Console tab

# WebSocket monitoring
npm run test:websocket
```

### Metrics to Monitor

```bash
# CPU Usage
top -p $(pgrep -f uvicorn)

# Memory Usage
ps aux | grep uvicorn

# Database Connections
psql -c "SELECT count(*) FROM pg_stat_activity;"

# Error Rate
tail -f logs/error.log | grep ERROR

# Response Time
tail -f logs/access.log | awk '{print $NF}'
```

### Logging Configuration

**Backend Logging (logging.conf):**
```ini
[loggers]
keys=root,timewarp

[handlers]
keys=console,file

[formatters]
keys=standard

[logger_root]
level=INFO
handlers=console,file

[logger_timewarp]
level=DEBUG
handlers=console,file
qualname=timewarp

[handler_console]
class=StreamHandler
level=DEBUG
formatter=standard
args=(sys.stdout,)

[handler_file]
class=FileHandler
level=DEBUG
formatter=standard
args=('logs/timewarp.log',)

[formatter_standard]
format=%(asctime)s [%(levelname)s] %(name)s: %(message)s
```

---

## Troubleshooting

### Common Issues

**Connection Refused**
```bash
# Check if service is running
lsof -i :8000  # Backend
lsof -i :3000  # Frontend

# Restart service
docker-compose restart api
npm run dev
```

**Database Connection Error**
```bash
# Check database connection
psql -h prod-db.example.com -U timewarp_user -d timewarp_prod -c "SELECT 1;"

# Check environment variables
echo $DATABASE_URL

# Reset database
python -m alembic downgrade base
python -m alembic upgrade head
```

**Memory Issues**
```bash
# Check memory usage
free -h
docker stats

# Increase limits
# Edit docker-compose.yml: mem_limit: 2g

# Restart with more memory
docker-compose down
docker-compose up -d
```

**WebSocket Connection Failed**
```bash
# Check WebSocket connectivity
npm run test:websocket

# Check firewall
sudo ufw allow 8000/tcp

# Check CORS configuration
# Verify CORS_ORIGINS in .env
```

### Debug Mode

```bash
# Enable debug logging
export DEBUG=True
python -m uvicorn main:app --reload

# Verbose npm output
npm run dev -- --verbose

# Database queries logging
# Add to .env: SQLALCHEMY_ECHO=True
```

---

**Last Updated:** December 31, 2025  
**Version:** 1.0  
**Status:** Production-Ready
