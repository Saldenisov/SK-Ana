#!/bin/bash
# ========================================
# SK-Ana Quick Docker Rebuild Script
# ========================================
# Fast rebuild using Docker cache
# Perfect for iterative development

echo ""
echo "========================================"
echo "SK-Ana Quick Rebuild"
echo "========================================"
echo ""

# Configuration
IMAGE_NAME="skana"
IMAGE_TAG="latest"
CONTAINER_NAME="skana"
PORT=3840
DOCKERFILE="Dockerfile"

# Step 1: Check if Docker is running
echo "[1/4] Checking Docker daemon..."
if ! docker info > /dev/null 2>&1; then
    echo "  ✗ Error: Docker daemon is not running"
    echo "  Please start Docker Desktop and try again"
    exit 1
fi
echo "  ✓ Docker is running"
echo ""

# Step 2: Stop and remove existing container if it exists
echo "[2/4] Checking for existing container..."
if docker ps -a --filter "name=${CONTAINER_NAME}" --format "{{.Names}}" | grep -q "^${CONTAINER_NAME}$"; then
    echo "  Found existing container: ${CONTAINER_NAME}"
    echo "  Stopping container..."
    docker stop "${CONTAINER_NAME}" > /dev/null 2>&1
    echo "  Removing container..."
    docker rm "${CONTAINER_NAME}" > /dev/null 2>&1
    echo "  ✓ Container removed"
else
    echo "  ✓ No existing container found"
fi
echo ""

# Step 3: Build the Docker image (using cache)
echo "[3/4] Building Docker image (using cache)..."
echo "  Image: ${IMAGE_NAME}:${IMAGE_TAG}"
echo "  Dockerfile: ${DOCKERFILE}"
echo ""

if ! docker build -t "${IMAGE_NAME}:${IMAGE_TAG}" -f "${DOCKERFILE}" .; then
    echo ""
    echo "  ✗ Error: Failed to build image"
    exit 1
fi
echo ""
echo "  ✓ Image built successfully"
echo ""

# Step 4: Run the container
echo "[4/4] Starting container..."
echo "  Container name: ${CONTAINER_NAME}"
echo "  Port mapping: ${PORT} (local) -> 3840 (container)"
echo ""

if ! docker run -d -p "${PORT}:3840" --name "${CONTAINER_NAME}" -e PORT=3840 "${IMAGE_NAME}:${IMAGE_TAG}"; then
    echo "  ✗ Error: Failed to start container"
    exit 1
fi
echo "  ✓ Container started successfully"
echo ""

# Wait a moment and verify
echo "Verifying container..."
sleep 3
if docker ps --filter "name=${CONTAINER_NAME}" --format "{{.Names}}" | grep -q "^${CONTAINER_NAME}$"; then
    echo "  ✓ Container is running"
else
    echo "  ✗ Container is not running"
    echo "  Checking logs..."
    docker logs "${CONTAINER_NAME}"
    exit 1
fi

echo ""
echo "========================================"
echo "✓ SK-Ana is ready!"
echo "========================================"
echo ""
echo "Access the application at:"
echo "  http://localhost:${PORT}"
echo ""
echo "Quick commands:"
echo "  View logs:     docker logs -f ${CONTAINER_NAME}"
echo "  Stop:          docker stop ${CONTAINER_NAME}"
echo ""
