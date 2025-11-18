# SK-Ana Docker Deployment Guide

## Quick Start

### Using Pre-built Images

**Option 1: Updated Image (Recommended)**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```

**Option 2: Original Image**
```bash
docker run -d -p 3840:3840 --name skana-original ppernot1/skana
```

Access at: **http://localhost:3840** or **http://127.0.0.1:3840**

---

## Docker Commands Reference

### Container Management

**View logs**
```bash
docker logs -f skana
```

**Stop container**
```bash
docker stop skana
```

**Start container**
```bash
docker start skana
```

**Restart container**
```bash
docker restart skana
```

**Remove container**
```bash
docker stop skana
docker rm skana
```

**View running containers**
```bash
docker ps
```

**View all containers (including stopped)**
```bash
docker ps -a
```

### Image Management

**Pull latest version**
```bash
docker pull saldenisov/skana:latest
```

**View all images**
```bash
docker images
```

**Remove image**
```bash
docker rmi saldenisov/skana:latest
```

---

## Building Custom Image

### Prerequisites
- [Docker Desktop](https://www.docker.com/products/docker-desktop) installed and running
- SK-Ana source code

### Build from Source

**Navigate to project directory**
```bash
cd E:\dev\SK-Ana
```

**Build image**
```bash
docker build -t skana:latest .
```

**Run your custom build**
```bash
docker run -d -p 3840:3840 --name skana -e PORT=3840 skana:latest
```

---

## Rebuild Scripts (Windows)

Three convenient batch scripts for development:

### 1. `rebuild-docker.bat` - Full Rebuild
Complete fresh build (removes old image)

**Usage:**
```cmd
rebuild-docker.bat
```

**Time:** ~3-5 minutes

### 2. `quick-rebuild.bat` - Fast Rebuild
Uses Docker cache for faster iteration

**Usage:**
```cmd
quick-rebuild.bat
```

**Time:** ~30 seconds - 2 minutes

### 3. `view-logs.bat` - View Logs
Stream live logs from running container

**Usage:**
```cmd
view-logs.bat
```

---

## Pushing to Docker Hub

### 1. Create Docker Hub Account
Sign up at https://hub.docker.com

### 2. Login to Docker Hub
```bash
docker login
```

### 3. Tag Your Image
```bash
docker tag skana:latest YOUR_USERNAME/skana:latest
```

### 4. Push to Docker Hub
```bash
docker push YOUR_USERNAME/skana:latest
```

### 5. Others Can Use Your Image
```bash
docker run -d -p 3840:3840 --name skana YOUR_USERNAME/skana:latest
```

---

## GitHub Container Registry Alternative

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
docker run -d -p 3840:3840 --memory="4g" --name skana skana:latest
```

### Volume Mounting (for persistent data)

**Mount local directory:**
```bash
docker run -d -p 3840:3840 -v E:\data:/SK-Ana/data --name skana skana:latest
```

---

## Image Information

| Property | saldenisov/skana | ppernot1/skana |
|----------|------------------|----------------|
| **Base** | rocker/shiny:4.4.1 | rocker/shiny |
| **R Version** | 4.4.1 | 4.2.x |
| **Size** | ~1.7 GB | ~1.5 GB |
| **Features** | Latest fixes, Correction Spectra | Original |
| **Status** | Actively maintained | Stable |

---

## Troubleshooting

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

### Port already in use
```bash
# Option 1: Stop existing container
docker stop skana
docker rm skana

# Option 2: Use different port
docker run -d -p 3841:3840 --name skana-new saldenisov/skana:latest
# Access at http://localhost:3841
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

### Dockerfile not found
```bash
# Ensure you're in the correct directory
cd E:\dev\SK-Ana

# Verify Dockerfile exists
dir Dockerfile
```

---

## Development Workflow

**Typical workflow after code changes:**

1. Make changes to R files (ui.R, server.R, etc.)
2. Run `quick-rebuild.bat` (Windows) or rebuild command
3. Wait ~1-2 minutes
4. Refresh browser at http://localhost:3840
5. Test changes
6. Repeat!

**For major changes (new packages, Dockerfile changes):**
- Use `rebuild-docker.bat` for a clean build

---

## Performance Notes

- **Build time (full):** ~2-5 minutes
- **Build time (cached):** ~30 seconds - 2 minutes
- **Startup time:** ~30-60 seconds
- **Memory usage:** ~1-2 GB base + dynamic (depends on analysis)
- **CPU usage:** Minimal idle, spikes during ALS/SVD calculations

---

## File Locations Inside Container

```
/SK-Ana/
├── app.R                          # Entry point
├── ui.R, server.R, global.R       # Main application
├── ui_files/                      # UI components
├── server_files/                  # Server logic
├── data/                          # Example datasets
├── docs/                          # Documentation
├── outputDir/                     # Results (runtime)
└── renv/                          # R dependencies
```

---

## Cleanup Commands

**Remove all SK-Ana containers:**
```bash
docker ps -a --filter "name=skana" --format "{{.Names}}" | ForEach-Object { docker rm -f $_ }
```

**Remove all SK-Ana images:**
```bash
docker images --filter "reference=*skana*" --format "{{.Repository}}:{{.Tag}}" | ForEach-Object { docker rmi $_ }
```

**Full cleanup (Docker system prune):**
```bash
docker system prune -a --volumes
# WARNING: Removes ALL unused images, containers, and volumes
```

---

## Advanced Usage

### Interactive Shell Access
```bash
docker exec -it skana bash
```

### Copy Files from Container
```bash
# Copy result files from container to local machine
docker cp skana:/SK-Ana/outputDir/. E:\results\
```

### Copy Files to Container
```bash
# Copy local files into container
docker cp E:\data\mydata.csv skana:/SK-Ana/data/
```

### Check R Package Versions
```bash
docker exec skana Rscript -e "sessionInfo()"
```

---

## Docker Compose (Optional)

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

## Security Considerations

**For production deployments:**

1. **Use HTTPS:** Configure reverse proxy (nginx, Traefik)
2. **Authentication:** Add Shiny authentication layer
3. **Network isolation:** Use Docker networks
4. **Resource limits:** Always set memory/CPU limits
5. **Updates:** Regularly pull latest images for security patches

**Example with nginx reverse proxy:**
```nginx
server {
    listen 443 ssl;
    server_name skana.yourdomain.com;
    
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    
    location / {
        proxy_pass http://localhost:3840;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 3600s;
    }
}
```

---

## Comparison: Local vs Docker

| Aspect | Local R Installation | Docker Container |
|--------|---------------------|------------------|
| **Setup** | Install R + packages manually | One command |
| **Portability** | OS-dependent | Works everywhere |
| **Isolation** | Can conflict with other R projects | Fully isolated |
| **Updates** | Manage packages individually | Pull new image |
| **Speed** | Native performance | ~5-10% overhead |
| **Disk space** | Smaller (~500 MB) | Larger (~1.7 GB) |

---

## Support

**Resources:**
- **GitHub Issues:** https://github.com/ppernot/SK-Ana/issues
- **Documentation:** `/SK-Ana/docs/` (inside container)
- **Docker Hub:** https://hub.docker.com/r/saldenisov/skana

**Report issues with:**
- Docker version: `docker --version`
- Container logs: `docker logs skana`
- Error messages and steps to reproduce

---

## Version History

- **saldenisov/skana:latest** - R 4.4.1, latest bug fixes, correction spectra support
- **ppernot1/skana** - Original stable version

---

**Last Updated:** 2025-11-18  
**Docker Image:** saldenisov/skana:latest  
**Status:** Production Ready ✅
