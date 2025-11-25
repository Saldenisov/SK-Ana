#!/bin/bash
# ========================================
# SK-Ana Docker Logs Viewer
# ========================================
# View live logs from running container

CONTAINER_NAME="skana"

echo ""
echo "========================================"
echo "SK-Ana Container Logs"
echo "========================================"
echo ""

# Check if container exists
if ! docker ps -a --filter "name=${CONTAINER_NAME}" --format "{{.Names}}" | grep -q "^${CONTAINER_NAME}$"; then
    echo "✗ Error: Container '${CONTAINER_NAME}' not found"
    echo ""
    echo "Available containers:"
    docker ps -a --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    echo ""
    exit 1
fi

# Check if container is running
if ! docker ps --filter "name=${CONTAINER_NAME}" --format "{{.Names}}" | grep -q "^${CONTAINER_NAME}$"; then
    echo "⚠ Warning: Container '${CONTAINER_NAME}' exists but is not running"
    echo ""
    echo "Container status:"
    docker ps -a --filter "name=${CONTAINER_NAME}" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    echo ""
    echo "Showing available logs (Ctrl+C to exit)..."
    echo ""
    docker logs "${CONTAINER_NAME}"
    exit 0
fi

echo "✓ Container '${CONTAINER_NAME}' is running"
echo "Streaming live logs (Ctrl+C to exit)..."
echo ""

docker logs -f "${CONTAINER_NAME}"
