#!/bin/bash
# Health check script for Time Warp Studio container
set -e

# Check if the Python backend is responding
curl -sf http://localhost:${PORT:-8000}/health || exit 1

echo "Health check passed"
