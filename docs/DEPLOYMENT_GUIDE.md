# Time Warp Studio - Comprehensive Deployment Guide

## Quick Start

### Using Docker Compose (Recommended)

```bash
# Clone repository
git clone https://github.com/honey-badger/Time_Warp_Studio.git
cd Time_Warp_Studio

# Build and run
docker-compose up -d

# View logs
docker-compose logs -f api

# Stop services
docker-compose down
```

Access at: http://localhost:8080

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Deployment Methods](#deployment-methods)
3. [Environment Configuration](#environment-configuration)
4. [Database Setup](#database-setup)
5. [SSL/HTTPS Configuration](#sslhttps-configuration)
6. [Monitoring & Logging](#monitoring--logging)
7. [Scaling & Performance](#scaling--performance)
8. [Troubleshooting](#troubleshooting)
9. [Security](#security)
10. [Backup & Recovery](#backup--recovery)

---

## Prerequisites

### System Requirements

- **Docker**: Version 20.10+ (Docker Engine and Docker Compose)
- **Git**: For cloning repository
- **Memory**: 4GB minimum (8GB recommended)
- **Disk**: 10GB free space
- **CPU**: 2 cores minimum (4+ recommended)
- **Network**: Ports 80, 443, 8000, 8080, 5432, 6379 available

### Software Stack

- **Frontend**: Node.js 20, React, TypeScript
- **Backend**: Python 3.11, FastAPI
- **Database**: PostgreSQL 15
- **Cache**: Redis 7
- **Web Server**: Nginx 1.25
- **Monitoring**: Prometheus, Grafana (optional)

### Required Tools

```bash
# Check Docker version
docker --version

# Check Docker Compose version
docker-compose --version

# Check Git version
git --version

# Check available disk space
df -h

# Check available memory
free -h
```

---

## Deployment Methods

### 1. Docker Compose (Development/Production)

#### Quick Deploy

```bash
docker-compose up -d
```

#### With Environment File

```bash
# Create .env file
cp .env.example .env
nano .env

# Deploy with environment
docker-compose --env-file .env up -d
```

#### With Specific Profile

```bash
# Production with monitoring
docker-compose --profile prod --profile monitoring up -d

# Development with frontend
docker-compose --profile dev up -d

# Just infrastructure (no app)
docker-compose up -d db cache
```

#### Verification

```bash
# Check running services
docker-compose ps

# Check logs
docker-compose logs api -f

# Test API
curl http://localhost:8000/health

# Test Web UI
curl http://localhost:8080
```

### 2. Kubernetes Deployment

#### Prerequisites

- `kubectl` installed and configured
- Kubernetes cluster (1.20+)
- Helm 3+ (recommended)

#### Using Helm

```bash
# Add Helm repository
helm repo add timewarp https://charts.timewarp.io
helm repo update

# Install release
helm install my-timewarp timewarp/time-warp-studio \
  --namespace timewarp \
  --create-namespace \
  --values values.yaml

# View status
helm status my-timewarp -n timewarp

# View deployed resources
kubectl get pods -n timewarp
kubectl get svc -n timewarp
```

#### Using kubectl (Manual)

```bash
# Create namespace
kubectl create namespace timewarp

# Create secrets
kubectl create secret generic db-credentials \
  --from-literal=username=timewarp \
  --from-literal=password=changeme \
  -n timewarp

# Apply manifests
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/secrets.yaml
kubectl apply -f k8s/pvc.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
kubectl apply -f k8s/ingress.yaml

# View status
kubectl get all -n timewarp

# View logs
kubectl logs -f deployment/api -n timewarp
```

#### Ingress Configuration

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: timewarp-ingress
  namespace: timewarp
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - timewarp.example.com
    secretName: timewarp-tls
  rules:
  - host: timewarp.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: api
            port:
              number: 8000
```

### 3. Cloud Deployments

#### Azure App Service

```bash
# Create resource group
az group create --name timewarp-rg --location eastus

# Create App Service plan
az appservice plan create \
  --name timewarp-plan \
  --resource-group timewarp-rg \
  --sku B2 \
  --is-linux

# Create Web App
az webapp create \
  --name timewarp-app \
  --resource-group timewarp-rg \
  --plan timewarp-plan \
  --runtime "python|3.11"

# Deploy from Docker
az webapp deployment container config \
  --name timewarp-app \
  --resource-group timewarp-rg

# Set container image
az webapp config container set \
  --name timewarp-app \
  --resource-group timewarp-rg \
  --docker-custom-image-name ghcr.io/honey-badger/time-warp-studio:latest
```

#### AWS ECS Fargate

```bash
# Create CloudFormation stack
aws cloudformation create-stack \
  --stack-name timewarp-stack \
  --template-body file://aws/cloudformation.yaml \
  --parameters ParameterKey=ContainerImage,ParameterValue=ghcr.io/honey-badger/time-warp-studio:latest

# View stack status
aws cloudformation describe-stacks --stack-name timewarp-stack
```

#### Google Cloud Run

```bash
# Deploy to Cloud Run
gcloud run deploy timewarp \
  --image ghcr.io/honey-badger/time-warp-studio:latest \
  --platform managed \
  --region us-central1 \
  --set-env-vars DATABASE_URL=...,REDIS_URL=...
```

---

## Environment Configuration

### Create .env File

```bash
cp .env.example .env
```

### Configure Variables

```env
# Application
ENVIRONMENT=production
DEBUG=false
LOG_LEVEL=info

# Database
DATABASE_URL=postgresql://timewarp:password@localhost:5432/timewarp
DATABASE_POOL_SIZE=20
DATABASE_MAX_OVERFLOW=10

# Redis
REDIS_URL=redis://localhost:6379/0
REDIS_PASSWORD=

# Security
SECRET_KEY=your-secret-key-here-min-32-chars
JWT_SECRET=your-jwt-secret-here
CORS_ORIGINS=https://example.com,https://app.example.com

# Email (optional)
SMTP_HOST=smtp.gmail.com
SMTP_PORT=587
SMTP_USER=your-email@gmail.com
SMTP_PASSWORD=your-app-password
SENDER_EMAIL=noreply@timewarp.io

# Storage
STORAGE_TYPE=s3  # or local, gcs, azure
S3_BUCKET=timewarp-uploads
S3_REGION=us-east-1
S3_ACCESS_KEY=
S3_SECRET_KEY=

# Analytics
ANALYTICS_PROVIDER=mixpanel  # or google, segment
MIXPANEL_TOKEN=
SENTRY_DSN=

# Feature Flags
FEATURE_FLAG_PROVIDER=local  # or launchdarkly, configcat
LAUNCHDARKLY_SDK_KEY=

# API Rate Limiting
RATE_LIMIT_ENABLED=true
RATE_LIMIT_REQUESTS=1000
RATE_LIMIT_PERIOD=3600

# API Keys
GOOGLE_OAUTH_CLIENT_ID=
GITHUB_OAUTH_CLIENT_ID=
GITHUB_OAUTH_CLIENT_SECRET=

# Nginx
NGINX_WORKER_PROCESSES=auto
NGINX_WORKER_CONNECTIONS=2048

# Monitoring
PROMETHEUS_ENABLED=true
PROMETHEUS_PORT=9090
GRAFANA_ENABLED=true
GRAFANA_ADMIN_PASSWORD=admin

# SSL/TLS
SSL_ENABLED=true
SSL_CERT_PATH=/etc/ssl/certs/cert.pem
SSL_KEY_PATH=/etc/ssl/private/key.pem
```

---

## Database Setup

### PostgreSQL Initialization

```bash
# Connect to PostgreSQL
psql -U postgres -h localhost

# Create database
CREATE DATABASE timewarp;

# Create user
CREATE USER timewarp WITH PASSWORD 'secure_password';

# Grant privileges
GRANT ALL PRIVILEGES ON DATABASE timewarp TO timewarp;

# Connect to database
\c timewarp

# Run migrations
python -m alembic upgrade head
```

### Database Migrations

```bash
# Create new migration
alembic revision --autogenerate -m "Add users table"

# Run migrations
alembic upgrade head

# View migration history
alembic history

# Rollback one migration
alembic downgrade -1

# Rollback to specific revision
alembic downgrade abc1234
```

### Redis Initialization

```bash
# Connect to Redis
redis-cli

# Set password (if not already set)
CONFIG SET requirepass your_password

# Verify connection
PING

# Check data
DBSIZE
```

---

## SSL/HTTPS Configuration

### Self-Signed Certificate (Development)

```bash
# Generate certificate
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365

# Update docker-compose.yml
volumes:
  - ./cert.pem:/etc/ssl/certs/cert.pem
  - ./key.pem:/etc/ssl/private/key.pem
```

### Let's Encrypt (Production)

```bash
# Install certbot
sudo apt-get install certbot python3-certbot-nginx

# Generate certificate
sudo certbot certonly --nginx -d example.com

# Auto-renewal
sudo systemctl enable certbot.timer
sudo systemctl start certbot.timer
```

### Update Nginx Configuration

```nginx
server {
    listen 443 ssl http2;
    server_name example.com;

    ssl_certificate /etc/letsencrypt/live/example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/example.com/privkey.pem;

    # SSL security
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;

    # HTTP/2 push
    http2_push_preload on;

    location / {
        proxy_pass http://api:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}

# Redirect HTTP to HTTPS
server {
    listen 80;
    server_name example.com;
    return 301 https://$server_name$request_uri;
}
```

---

## Monitoring & Logging

### Prometheus Setup

```yaml
# prometheus.yml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'api'
    static_configs:
      - targets: ['localhost:8000']

  - job_name: 'postgres'
    static_configs:
      - targets: ['localhost:5432']

  - job_name: 'redis'
    static_configs:
      - targets: ['localhost:6379']
```

### Grafana Dashboards

```bash
# Default dashboards
- Docker Container Metrics
- PostgreSQL Metrics
- Redis Metrics
- API Performance
- Error Rate Monitoring
```

### Logging

```bash
# View centralized logs
docker-compose logs -f

# View specific service logs
docker-compose logs -f api
docker-compose logs -f db

# Export logs
docker-compose logs api > api-logs.txt

# Filter logs
docker-compose logs api | grep ERROR
docker-compose logs api | grep -A 5 "Traceback"
```

### Structured Logging

```python
import logging
import json

logging.basicConfig(
    format='%(message)s',
    level=logging.INFO
)

logger = logging.getLogger(__name__)

# Log with context
logger.info(json.dumps({
    "event": "code_executed",
    "user_id": "usr_123",
    "duration_ms": 125,
    "success": True,
}))
```

---

## Scaling & Performance

### Horizontal Scaling

```bash
# Scale API instances with Docker Compose
docker-compose up -d --scale api=3

# Load balancing happens automatically via Nginx
```

### Vertical Scaling

```bash
# Increase resource limits in docker-compose.yml
services:
  api:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G
```

### Database Optimization

```sql
-- Create indexes
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_projects_owner ON projects(owner_id);
CREATE INDEX idx_executions_created ON executions(created_at);

-- Analyze query plans
EXPLAIN ANALYZE SELECT * FROM projects WHERE owner_id = $1;

-- Vacuum and analyze
VACUUM ANALYZE;
```

### Caching Strategy

```python
# Redis caching
from functools import lru_cache
import redis

cache = redis.Redis(host='localhost', port=6379, db=0)

def cache_key(func_name, *args):
    return f"{func_name}:{':'.join(map(str, args))}"

def cached(ttl=3600):
    def decorator(func):
        def wrapper(*args, **kwargs):
            key = cache_key(func.__name__, *args)
            value = cache.get(key)
            if value:
                return json.loads(value)
            
            result = func(*args, **kwargs)
            cache.setex(key, ttl, json.dumps(result))
            return result
        return wrapper
    return decorator
```

---

## Troubleshooting

### Connection Issues

```bash
# Check port availability
lsof -i :8000
lsof -i :5432
lsof -i :6379

# Test connectivity
curl http://localhost:8000/health
telnet localhost 5432
redis-cli ping

# Check Docker network
docker network ls
docker network inspect timewarp-network
```

### Database Issues

```bash
# Check PostgreSQL logs
docker-compose logs db | tail -50

# Connect and test
docker-compose exec db psql -U timewarp -d timewarp -c "SELECT version();"

# Check connections
SELECT * FROM pg_stat_activity;

# Kill idle connections
SELECT pid, usename, pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE idle_in_transaction AND query_start < now() - interval '5 minutes';
```

### Memory Leaks

```bash
# Monitor memory usage
docker stats

# Check Docker memory limits
docker inspect <container-id> | grep -A 10 Memory

# Analyze with memory profiler
python -m memory_profiler script.py
```

### Performance Degradation

```bash
# Check system resources
top
free -h
df -h
iostat -x 1 5

# Check database slow queries
SET log_min_duration_statement = 1000;  # 1 second

# Check application metrics
curl http://localhost:9090/metrics
```

---

## Security

### Secrets Management

```bash
# Using environment variables
export DATABASE_PASSWORD=$(head -c 32 /dev/urandom | base64)

# Using Docker secrets
docker secret create db_password -

# Using key management service
aws secretsmanager create-secret --name timewarp/db-password
```

### Network Security

```yaml
# docker-compose.yml
networks:
  timewarp:
    driver: bridge
    driver_opts:
      com.docker.network.bridge.enable_ip_masquerade: "true"

services:
  api:
    networks:
      timewarp:
        ipv4_address: 172.20.0.2
  db:
    networks:
      timewarp:
        ipv4_address: 172.20.0.3
```

### Firewall Rules

```bash
# UFW (Ubuntu)
sudo ufw allow 22/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw allow from 10.0.0.0/8  # Internal network
sudo ufw enable

# iptables
sudo iptables -A INPUT -p tcp --dport 22 -j ACCEPT
sudo iptables -A INPUT -p tcp --dport 80 -j ACCEPT
sudo iptables -A INPUT -p tcp --dport 443 -j ACCEPT
```

---

## Backup & Recovery

### Database Backup

```bash
# Full backup
docker-compose exec db pg_dump -U timewarp timewarp > backup.sql

# Compressed backup
docker-compose exec db pg_dump -U timewarp timewarp | gzip > backup.sql.gz

# Backup with custom format (faster restore)
docker-compose exec db pg_dump -U timewarp -F c -f backup.dump timewarp
```

### Database Restore

```bash
# From SQL dump
docker-compose exec -T db psql -U timewarp timewarp < backup.sql

# From compressed dump
gunzip < backup.sql.gz | docker-compose exec -T db psql -U timewarp timewarp

# From custom format
docker-compose exec db pg_restore -U timewarp -d timewarp backup.dump
```

### Redis Backup

```bash
# Manual backup
docker-compose exec redis redis-cli BGSAVE

# Copy dump file
docker cp <container-id>:/data/dump.rdb ./backup/

# Restore
docker cp ./backup/dump.rdb <container-id>:/data/dump.rdb
```

### Full System Backup

```bash
# Backup everything
docker-compose exec api tar czf - /app > app-backup.tar.gz
docker-compose exec db pg_dump -U timewarp timewarp | gzip > db-backup.sql.gz
docker cp <redis-id>:/data/dump.rdb ./redis-backup.rdb

# Automated backup script
#!/bin/bash
BACKUP_DIR="/backups"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p $BACKUP_DIR/$DATE
docker-compose exec db pg_dump -U timewarp timewarp | gzip > $BACKUP_DIR/$DATE/db.sql.gz
docker-compose exec api tar czf $BACKUP_DIR/$DATE/app.tar.gz /app
docker cp <redis-id>:/data/dump.rdb $BACKUP_DIR/$DATE/redis.rdb

# Keep only last 30 days
find $BACKUP_DIR -maxdepth 1 -type d -mtime +30 -exec rm -rf {} \;
```

---

## Monitoring Health

### Health Checks

```bash
# API health
curl http://localhost:8000/health

# Database health
curl http://localhost:8000/health/db

# Redis health
curl http://localhost:8000/health/redis

# Overall system health
curl http://localhost:8000/health/system
```

### Metrics Endpoints

```bash
# Prometheus metrics
curl http://localhost:8000/metrics

# Application stats
curl http://localhost:8000/stats

# Performance report
curl http://localhost:8000/performance
```

---

## Summary Checklist

- [ ] System requirements met
- [ ] Git repository cloned
- [ ] `.env` file created and configured
- [ ] Docker and Docker Compose installed
- [ ] Database initialized
- [ ] Redis initialized
- [ ] Deployment method selected
- [ ] Services running: `docker-compose ps`
- [ ] Health checks passing
- [ ] SSL/HTTPS configured
- [ ] Monitoring enabled
- [ ] Backup strategy implemented
- [ ] Security hardened
- [ ] Load testing completed
- [ ] Documentation reviewed

---

## Support

- **Issues**: https://github.com/honey-badger/Time_Warp_Studio/issues
- **Documentation**: https://docs.timewarp.io
- **Community**: https://community.timewarp.io
- **Email**: support@timewarp.io

---

**Last Updated**: 2024-01-15  
**Version**: 5.1.0
