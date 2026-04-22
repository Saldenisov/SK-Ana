# SK-Ana Docker Deployment Guide

Cross-platform Docker deployment instructions for Windows, Mac, and Linux.

## Table of Contents
- [Quick Start](#quick-start)
- [Platform-Specific Scripts](#platform-specific-scripts)
- [Manual Docker Commands](#manual-docker-commands)
- [Building from Source](#building-from-source)
- [Configuration](#configuration)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

### Prerequisites
- [Docker Desktop](https://www.docker.com/products/docker-desktop) installed and running
  - **Windows**: Docker Desktop for Windows
  - **Mac**: Docker Desktop for Mac (Intel or Apple Silicon)
  - **Linux**: Docker Engine or Docker Desktop

### Run Pre-built Image - Platform Specific

#### Windows & Linux Users (amd64/x86_64)

**Using the updated image (Recommended):**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```

**Access the application:**
- Open your browser and go to: **http://localhost:3840**

#### Mac Users

**Intel Macs (x86_64):**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```

**Apple Silicon Macs (M1/M2/M3 - arm64):**

You have two options:

**Option A: Use emulation (quick, works immediately)**
```bash
# Works but uses emulation (slightly slower)
docker run -d -p 3840:3840 --platform linux/amd64 --name skana saldenisov/skana:latest
```

**Option B: Build native arm64 (recommended for best performance)**
```bash
# Clone/download repository first, then:
cd /path/to/SK-Ana
docker build --platform linux/arm64 -f Dockerfile.arm64 -t skana:arm64 .
docker run -d -p 3840:3840 -e PORT=3840 --name skana skana:arm64
```

**Benefits of native arm64 build:**
- 10-30% better performance (no emulation)
- No platform mismatch warnings
- Native Apple Silicon optimization

**Access the application:**
- Open your browser and go to: **http://localhost:3840**

**Stop the container:**
```bash
docker stop skana
```

**Restart the container:**
```bash
docker restart skana
```

**Remove the container:**
```bash
docker stop skana
docker rm skana
```

---

## Platform-Specific Scripts

Convenient scripts are provided for each platform to rebuild and manage Docker containers during development.

### Windows Scripts (.bat)

Located in the project root:
- `rebuild-docker.bat` - Full rebuild (removes old image, ~3-5 minutes)
- `quick-rebuild.bat` - Fast rebuild using cache (~30 seconds - 2 minutes)
- `view-logs.bat` - View live container logs
- `push.bat` - Push image to Docker Hub

**Usage:**
```cmd
# Full rebuild
rebuild-docker.bat

# Quick rebuild (after code changes)
quick-rebuild.bat

# View logs
view-logs.bat

# Push to Docker Hub
push.bat
```

### Mac/Linux Scripts (.sh)

Located in the project root:
- `rebuild-docker.sh` - Full rebuild (removes old image, ~3-5 minutes)
- `quick-rebuild.sh` - Fast rebuild using cache (~30 seconds - 2 minutes)
- `view-logs.sh` - View live container logs
- `push.sh` - Push image to Docker Hub

**First time setup (make scripts executable):**
```bash
chmod +x rebuild-docker.sh quick-rebuild.sh view-logs.sh push.sh
```

**Usage:**
```bash
# Full rebuild
./rebuild-docker.sh

# Quick rebuild (after code changes)
./quick-rebuild.sh

# View logs
./view-logs.sh

# Push to Docker Hub
./push.sh
```

---

## Manual Docker Commands

If you prefer manual control or the scripts don't work for your setup:

### Container Management

**Stop container:**
```bash
docker stop skana
```

**Start container:**
```bash
docker start skana
```

**Restart container:**
```bash
docker restart skana
```

**Remove container:**
```bash
docker stop skana
docker rm skana
```

**View logs:**
```bash
# Live logs (Ctrl+C to exit)
docker logs -f skana

# Last 100 lines
docker logs --tail 100 skana
```

**View running containers:**
```bash
docker ps
```

**View all containers (including stopped):**
```bash
docker ps -a
```

### Image Management

**Pull latest version:**
```bash
docker pull saldenisov/skana:latest
```

**View all images:**
```bash
docker images
```

**Remove image:**
```bash
docker rmi saldenisov/skana:latest
```

---

## Building from Source

### Build Custom Image - Platform Specific

#### Windows/Linux/Intel Mac (amd64)

**Navigate to project directory:**
```bash
# Windows
cd C:\path\to\SK-Ana

# Mac/Linux  
cd /path/to/SK-Ana
```

**Build image:**
```bash
docker build -t skana:latest -f Dockerfile .
```

**Run your custom build:**
```bash
docker run -d -p 3840:3840 --name skana -e PORT=3840 skana:latest
```

#### Mac Apple Silicon (M1/M2/M3 - arm64)

**Navigate to project directory:**
```bash
cd /path/to/SK-Ana
```

**Build native arm64 image:**
```bash
docker build --platform linux/arm64 -f Dockerfile.arm64 -t skana:arm64 .
```

**Run your custom arm64 build:**
```bash
docker run -d -p 3840:3840 -e PORT=3840 --name skana skana:arm64
```

**Why use Dockerfile.arm64 on Apple Silicon?**
- Built on `r-base:4.4.1` which natively supports arm64
- No emulation overhead (faster execution)
- No platform mismatch warnings
- Better resource utilization

### Available Dockerfiles

| Dockerfile | Architecture | Base Image | Best For |
|------------|--------------|------------|----------|
| `Dockerfile` | linux/amd64 (x86_64) | rocker/shiny:4.4.1 | Windows, Linux, Intel Mac |
| `Dockerfile.arm64` | linux/arm64 | r-base:4.4.1 | Mac Apple Silicon (M1/M2/M3) |

### Push to Docker Hub

**1. Login to Docker Hub:**
```bash
docker login
```

**2. Tag your image:**
```bash
# Replace YOUR_USERNAME with your Docker Hub username
docker tag skana:latest YOUR_USERNAME/skana:latest
```

**3. Push to Docker Hub:**
```bash
docker push YOUR_USERNAME/skana:latest
```

**4. Others can now use your image:**
```bash
docker run -d -p 3840:3840 --name skana YOUR_USERNAME/skana:latest
```

### Alternative: GitHub Container Registry

```bash
# Tag for GitHub Container Registry
docker tag skana:latest ghcr.io/YOUR_GITHUB_USERNAME/skana:latest

# Login (requires personal access token)
docker login ghcr.io -u YOUR_GITHUB_USERNAME

# Push
docker push ghcr.io/YOUR_GITHUB_USERNAME/skana:latest

# Run from GitHub
docker run -d -p 3840:3840 --name skana ghcr.io/YOUR_GITHUB_USERNAME/skana:latest
```

---

## Configuration

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| PORT | 3840 | Shiny application port |

**Example with custom port:**
```bash
docker run -d -p 8080:3840 --name skana -e PORT=3840 skana:latest
# Access at http://localhost:8080
```

### Memory Limits

**Set memory limit:**
```bash
docker run -d -p 3840:3840 --memory="4g" --name skana saldenisov/skana:latest
```

### Volume Mounting

**Mount local directory for persistent data:**

**Windows:**
```cmd
docker run -d -p 3840:3840 -v E:\data:/SK-Ana/data --name skana saldenisov/skana:latest
```

**Mac/Linux:**
```bash
docker run -d -p 3840:3840 -v ~/data:/SK-Ana/data --name skana saldenisov/skana:latest
```

### Docker Compose (Optional)

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  skana:
    image: saldenisov/skana:latest
    container_name: skana
    ports:
      - "3840:3840"
    environment:
      - PORT=3840
    restart: unless-stopped
    mem_limit: 4g
```

**Usage:**
```bash
# Start
docker-compose up -d

# Stop
docker-compose down

# View logs
docker-compose logs -f
```

---

## Troubleshooting

### Platform mismatch warning (Mac Apple Silicon)

**Warning message:**
```
WARNING: The requested image's platform (linux/amd64) does not match 
the detected host platform (linux/arm64/v8) and no specific platform was requested
```

**What this means:**
- The container runs via emulation (Rosetta 2)
- It works correctly but with ~10-20% performance overhead
- This is expected when using amd64 images on Apple Silicon

**How to fix:**
1. Stop the current container:
   ```bash
   docker stop skana
   docker rm skana
   ```

2. Build native arm64 image:
   ```bash
   cd /path/to/SK-Ana
   docker build --platform linux/arm64 -f Dockerfile.arm64 -t skana:arm64 .
   ```

3. Run native container:
   ```bash
   docker run -d -p 3840:3840 -e PORT=3840 --name skana skana:arm64
   ```

4. Verify (no warning should appear):
   ```bash
   docker ps --filter "name=skana"
   ```

### Docker daemon not running

**Error:** `Cannot connect to the Docker daemon`

**Fix:**
- **Windows/Mac**: Start Docker Desktop
- **Linux**: `sudo systemctl start docker`

### Port already in use

**Error:** `Bind for 0.0.0.0:3840 failed: port is already allocated`

**Fix:**
```bash
# Option 1: Stop existing container
docker stop skana
docker rm skana

# Option 2: Use different port
docker run -d -p 3841:3840 --name skana-new saldenisov/skana:latest
# Access at http://localhost:3841
```

### Container won't start

```bash
# Check logs
docker logs skana

# Check status
docker ps -a

# Remove and recreate
docker rm skana
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```

### Application not responding

```bash
# Restart container
docker restart skana

# Monitor startup
docker logs -f skana

# Check if Docker Desktop is running
# Ensure port 3840 is not blocked by firewall
```

### Build fails

**Check:**
1. Docker Desktop is running
2. Sufficient disk space (>5GB free)
3. No network issues (pulling base images)
4. Review error message in build output

### Memory issues

```bash
# Increase memory limit
docker run -d -p 3840:3840 --memory="4g" --name skana saldenisov/skana:latest

# Check Docker Desktop settings → Resources → Memory
```

### Permission issues (Linux)

```bash
# Add user to docker group
sudo usermod -aG docker $USER

# Log out and log back in, or run:
newgrp docker

# Make scripts executable
chmod +x *.sh
```

---

## Development Workflow

**Typical workflow after code changes:**

1. Make changes to R files (ui.R, server.R, etc.)
2. Run quick rebuild script:
   - **Windows**: `quick-rebuild.bat`
   - **Mac/Linux**: `./quick-rebuild.sh`
3. Wait ~1-2 minutes
4. Refresh browser at http://localhost:3840
5. Test changes
6. Repeat!

**For major changes (new packages, Dockerfile changes):**
- Use full rebuild script:
  - **Windows**: `rebuild-docker.bat`
  - **Mac/Linux**: `./rebuild-docker.sh`

---

## Advanced Usage

### Interactive Shell Access

```bash
docker exec -it skana bash
```

### Copy Files from Container

```bash
# Copy result files from container to local machine
docker cp skana:/SK-Ana/outputDir/. ./results/
```

### Copy Files to Container

```bash
# Copy local files into container
docker cp ./mydata.csv skana:/SK-Ana/data/
```

### Check R Package Versions

```bash
docker exec skana Rscript -e "sessionInfo()"
```

---

## Image Information

| Property | saldenisov/skana | skana:arm64 (build from source) | ppernot1/skana |
|----------|------------------|--------------------------------|----------------|
| **Base** | rocker/shiny:4.4.1 | r-base:4.4.1 | rocker/shiny |
| **R Version** | 4.4.1 | 4.4.1 | 4.2.x |
| **Architecture** | linux/amd64 | linux/arm64 | linux/amd64 |
| **Size** | ~1.7 GB | ~1.5 GB | ~1.5 GB |
| **Features** | Latest fixes, Correction Spectra | Native arm64, Latest fixes | Original |
| **Best For** | Windows, Linux, Intel Mac | Mac Apple Silicon | Legacy |
| **Status** | Actively maintained | Build-it-yourself | Stable |

---

## Performance Notes

- **Build time (full):** ~2-5 minutes
- **Build time (cached):** ~30 seconds - 2 minutes
- **Startup time:** ~30-60 seconds
- **Memory usage:** ~1-2 GB base + dynamic (depends on analysis)
- **CPU usage:** Minimal idle, spikes during ALS/SVD calculations

---

## Cleanup Commands

### Remove all SK-Ana containers

**Windows (PowerShell):**
```powershell
docker ps -a --filter "name=skana" --format "{{.Names}}" | ForEach-Object { docker rm -f $_ }
```

**Mac/Linux:**
```bash
docker ps -a --filter "name=skana" --format "{{.Names}}" | xargs docker rm -f
```

### Remove all SK-Ana images

**Windows (PowerShell):**
```powershell
docker images --filter "reference=*skana*" --format "{{.Repository}}:{{.Tag}}" | ForEach-Object { docker rmi $_ }
```

**Mac/Linux:**
```bash
docker images --filter "reference=*skana*" --format "{{.Repository}}:{{.Tag}}" | xargs docker rmi
```

### Full cleanup (Docker system prune)

```bash
docker system prune -a --volumes
# WARNING: Removes ALL unused images, containers, and volumes
```

---

## Support

**Resources:**
- **GitHub Repository:** https://github.com/Saldenisov/SK-Ana
- **Docker Hub:** https://hub.docker.com/r/saldenisov/skana
- **Documentation:** In repository docs/

**Report issues with:**
- Docker version: `docker --version`
- Platform: Windows/Mac/Linux
- Container logs: `docker logs skana`
- Error messages and steps to reproduce

---

**Last Updated:** 2025-11-25  
**Docker Image:** saldenisov/skana:latest  
**Status:** Production Ready ✅
