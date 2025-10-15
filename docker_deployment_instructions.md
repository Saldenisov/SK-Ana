# Docker Hub Deployment Instructions

Your custom SK-Ana Docker image has been built successfully and is running locally!

## Current Status
- ✅ Docker image built: `skana-custom`
- ✅ Container tested and running on port 3840
- ✅ All dependencies included (including missing `changepoint` package)

## To Push to Docker Hub

### 1. Get your Docker Hub username
If you don't have a Docker Hub account, create one at https://hub.docker.com

### 2. Tag your image for Docker Hub
Replace `YOUR_USERNAME` with your actual Docker Hub username:
```bash
docker tag skana-custom YOUR_USERNAME/skana
```

### 3. Login to Docker Hub
```bash
docker login
```
Enter your Docker Hub username and password when prompted.

### 4. Push to Docker Hub
```bash
docker push YOUR_USERNAME/skana
```

### 5. Test your remote image
After pushing, anyone can use your image with:
```bash
docker run -d -p 3840:3840 --name skana YOUR_USERNAME/skana
```

## Alternative: GitHub Container Registry

You can also use GitHub Container Registry instead:

```bash
# Tag for GitHub Container Registry
docker tag skana-custom ghcr.io/YOUR_GITHUB_USERNAME/skana

# Login to GitHub Container Registry (requires personal access token)
docker login ghcr.io -u YOUR_GITHUB_USERNAME

# Push to GitHub Container Registry
docker push ghcr.io/YOUR_GITHUB_USERNAME/skana

# Run from GitHub Container Registry
docker run -d -p 3840:3840 --name skana ghcr.io/YOUR_GITHUB_USERNAME/skana
```

## Cleanup Commands
```bash
# Stop the test container
docker stop test-skana-custom

# Remove the test container
docker rm test-skana-custom

# Remove local images (optional)
docker rmi skana-custom
```

Your SK-Ana application is now ready to be shared via Docker Hub!