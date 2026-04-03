#!/bin/bash

#############################################
# Time Warp Studio - Production Deployment Script
# Handles all deployment tasks for Phase 4.5
# Usage: ./deploy.sh [backend|all] [dev|staging|prod]
#############################################

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
REGISTRY="registry.example.com"
APP_NAME="time-warp"
VERSION=$(cat VERSION)
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="backups/$TIMESTAMP"

# Parse arguments
TARGET=${1:-all}
ENVIRONMENT=${2:-dev}

# Validate arguments
if [[ ! "$TARGET" =~ ^(backend|all)$ ]]; then
    echo -e "${RED}Error: Invalid target. Must be: backend or all${NC}"
    exit 1
fi

if [[ ! "$ENVIRONMENT" =~ ^(dev|staging|prod)$ ]]; then
    echo -e "${RED}Error: Invalid environment. Must be: dev, staging, or prod${NC}"
    exit 1
fi

#############################################
# Functions
#############################################

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        command -v docker >/dev/null 2>&1 || { log_error "Docker is required"; exit 1; }
        command -v kubectl >/dev/null 2>&1 || { log_error "Kubectl is required"; exit 1; }
    fi
    
    log_success "All prerequisites met"
}

# Run tests
run_tests() {
    log_info "Running tests..."
    
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        log_info "Running backend tests..."
        cd Platforms/backend
        python -m pytest tests/ -v --tb=short --cov=. --cov-report=html
        cd ../..
        log_success "Backend tests passed"
    fi
}

# Run security audit
run_security_audit() {
    log_info "Running security audit..."
    
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        cd Platforms/backend
        python -m pytest tests/test_security_audit.py -v
        python -m bandit -r . -f json -o bandit-report.json || true
        cd ../..
    fi
    
    log_success "Security audit completed"
}

# Run performance tests
run_performance_tests() {
    log_info "Running performance tests..."
    
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        cd Platforms/backend
        python -m pytest tests/test_load_performance.py -v -m performance
        cd ../..
    fi
    
    if [[ "$TARGET" =~ ^(frontend|all)$ ]]; then
        cd Platforms/web
        npm run perf-test || true
        cd ../..
    fi
    
    log_success "Performance tests completed"
}

# Build backend
build_backend() {
    log_info "Building backend Docker image..."
    
    cd Platforms/backend
    
    # Build image
    docker build \
        --tag "$REGISTRY/$APP_NAME-api:$VERSION" \
        --tag "$REGISTRY/$APP_NAME-api:latest" \
        --build-arg ENVIRONMENT="$ENVIRONMENT" \
        .
    
    log_success "Backend image built: $REGISTRY/$APP_NAME-api:$VERSION"
    
    cd ../..
}

# Backup database
backup_database() {
    log_info "Backing up database..."
    
    mkdir -p "$BACKUP_DIR"
    
    if [[ "$ENVIRONMENT" == "prod" ]]; then
        # Production backup
        pg_dump "${DATABASE_URL}" | gzip > "$BACKUP_DIR/db_prod_$TIMESTAMP.sql.gz"
        log_success "Database backed up to $BACKUP_DIR/db_prod_$TIMESTAMP.sql.gz"
    elif [[ "$ENVIRONMENT" == "staging" ]]; then
        pg_dump "${STAGING_DATABASE_URL}" | gzip > "$BACKUP_DIR/db_staging_$TIMESTAMP.sql.gz"
        log_success "Database backed up to $BACKUP_DIR/db_staging_$TIMESTAMP.sql.gz"
    fi
}

# Push images to registry
push_images() {
    log_info "Pushing images to registry..."
    
    docker push "$REGISTRY/$APP_NAME-api:$VERSION"
    docker push "$REGISTRY/$APP_NAME-api:latest"
    
    log_success "Images pushed to registry"
}

# Deploy to Kubernetes
deploy_backend_k8s() {
    log_info "Deploying backend to Kubernetes ($ENVIRONMENT)..."
    
    # Select namespace based on environment
    NAMESPACE="$ENVIRONMENT"
    
    # Apply manifests
    kubectl apply -f Platforms/backend/k8s/config.yaml -n "$NAMESPACE"
    kubectl apply -f Platforms/backend/k8s/deployment.yaml -n "$NAMESPACE"
    kubectl apply -f Platforms/backend/k8s/service.yaml -n "$NAMESPACE"
    
    # Update image
    kubectl set image \
        deployment/time-warp-api \
        api="$REGISTRY/$APP_NAME-api:$VERSION" \
        -n "$NAMESPACE"
    
    # Wait for rollout
    kubectl rollout status deployment/time-warp-api -n "$NAMESPACE" --timeout=5m
    
    log_success "Backend deployed to Kubernetes ($NAMESPACE)"
}

# Run health checks
health_check() {
    log_info "Running health checks..."
    
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        log_info "Checking API health..."
        
        # Determine API endpoint
        if [[ "$ENVIRONMENT" == "prod" ]]; then
            API_URL="https://api.timewarp.io"
        elif [[ "$ENVIRONMENT" == "staging" ]]; then
            API_URL="https://api-staging.timewarp.io"
        else
            API_URL="http://localhost:8000"
        fi
        
        # Check health endpoint
        if curl -sf "$API_URL/api/health" > /dev/null; then
            log_success "API health check passed"
        else
            log_error "API health check failed"
            return 1
        fi
        
        # Check database
        if curl -sf "$API_URL/api/health/db" > /dev/null; then
            log_success "Database health check passed"
        else
            log_error "Database health check failed"
            return 1
        fi
    fi
    
    log_success "All health checks passed"
}

# Rollback deployment
rollback() {
    log_warn "Rolling back deployment..."
    
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        kubectl rollout undo deployment/time-warp-api -n "$ENVIRONMENT"
        kubectl rollout status deployment/time-warp-api -n "$ENVIRONMENT" --timeout=5m
        log_success "Backend rolled back"
    fi
    
    log_success "Rollback completed"
}

# Main deployment flow
main() {
    log_info "=========================================="
    log_info "Time Warp Studio Deployment"
    log_info "Target: $TARGET | Environment: $ENVIRONMENT | Version: $VERSION"
    log_info "=========================================="
    
    # Check prerequisites
    check_prerequisites
    
    # Run tests
    run_tests
    
    # Security audit
    run_security_audit
    
    # Performance tests
    run_performance_tests
    
    # Backup database
    backup_database
    
    # Build
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        build_backend
        push_images
    fi
    
    if [[ "$TARGET" =~ ^(frontend|all)$ ]]; then
    fi
    
    if [[ "$TARGET" =~ ^(mobile|all)$ ]]; then
    fi
    
    # Deploy
    if [[ "$TARGET" =~ ^(backend|all)$ ]]; then
        deploy_backend_k8s
    fi
    
    if [[ "$TARGET" =~ ^(frontend|all)$ ]]; then
    fi
    
    if [[ "$TARGET" =~ ^(mobile|all)$ ]]; then
    fi
    
    # Health checks
    health_check || {
        log_error "Health checks failed, rolling back..."
        rollback
        exit 1
    }
    
    log_success "=========================================="
    log_success "Deployment completed successfully!"
    log_success "=========================================="
}

# Run main
main
