#!/bin/bash
# ========================================
# SK-Ana Docker Push Script
# ========================================
# Push image to Docker Hub

IMAGE_NAME="saldenisov/skana"
IMAGE_TAG="latest"

echo "Checking if repository exists..."
if docker manifest inspect "${IMAGE_NAME}:${IMAGE_TAG}" > /dev/null 2>&1; then
    echo "✓ Repository ${IMAGE_NAME} is accessible"
else
    echo "⚠ Repository ${IMAGE_NAME} does not exist or is not accessible"
    echo "Attempting to push anyway - Docker Hub will create it if you have permissions..."
fi

echo ""
echo "Pushing ${IMAGE_NAME}:${IMAGE_TAG}..."
if docker push "${IMAGE_NAME}:${IMAGE_TAG}"; then
    echo ""
    echo "✓ Successfully pushed to ${IMAGE_NAME}:${IMAGE_TAG}"
else
    echo ""
    echo "✗ Failed to push. Make sure you are logged in and have permissions."
    echo "Run: docker login"
    exit 1
fi
