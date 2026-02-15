# Multi-stage Docker build for Time Warp Studio

# Stage 1: Build Python backend
FROM python:3.11-slim as backend-builder
WORKDIR /app
COPY Platforms/Python/requirements.txt ./
RUN pip install --user --no-cache-dir -r requirements.txt
COPY Platforms/Python/ ./
RUN python -m py_compile core/*.py handlers/*.py || true

# Stage 2: Production runtime
FROM python:3.11-slim
LABEL maintainer="Time Warp Studio <james@honey-badger.org>"
LABEL version="7.0.0"
LABEL description="Time Warp Studio - Educational Multi-Language Programming Environment"

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    git \
    postgresql-client \
    redis-tools \
    supervisor \
    nginx \
    && rm -rf /var/lib/apt/lists/*

# Create application user
RUN useradd -m -u 1000 timewarp && \
    mkdir -p /app /var/log/timewarp /var/run/timewarp && \
    chown -R timewarp:timewarp /app /var/log/timewarp /var/run/timewarp

WORKDIR /app

# Copy Python dependencies
COPY --from=backend-builder /root/.local /home/timewarp/.local
ENV PATH=/home/timewarp/.local/bin:$PATH

# Copy backend code
COPY --chown=timewarp:timewarp Platforms/Python/ ./backend/

# Configuration files
COPY docker/nginx.conf /etc/nginx/nginx.conf
COPY docker/supervisord.conf /etc/supervisor/conf.d/timewarp.conf
COPY docker/health-check.sh ./
RUN chmod +x ./health-check.sh

# Environment variables
ENV PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PORT=8000 \
    WORKERS=4 \
    DATABASE_URL=postgresql://timewarp:timewarp@db:5432/timewarp \
    REDIS_URL=redis://cache:6379/0 \
    DEBUG=false

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD ./health-check.sh || exit 1

# Switch to non-root user
USER timewarp

# Expose ports
EXPOSE 8000 8080

# Start services
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/timewarp.conf"]
