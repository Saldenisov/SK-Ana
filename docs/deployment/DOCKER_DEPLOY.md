# SK-Ana Docker Deployment Guide

## Image Information

**Docker Hub Repository:** `saldenisov/skana:latest`  
**Image Size:** ~1.68 GB  
**Base Image:** rocker/shiny:4.4.1  
**Latest Update:** 2025-11-19 (Added PCA initialization)

## Quick Start

### Pull and Run

```bash
# Pull the latest image
docker pull saldenisov/skana:latest

# Run on port 3840
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest

# Access at: http://localhost:3840
```

### Custom Port

```bash
# Run on custom port (e.g., 8080)
docker run -d -p 8080:3840 --name skana -e PORT=3840 saldenisov/skana:latest

# Access at: http://localhost:8080
```

## Windows PowerShell

```powershell
# Pull the image
docker pull saldenisov/skana:latest

# Run the container
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest

# Check if running
docker ps

# View logs
docker logs -f skana

# Stop
docker stop skana

# Remove
docker rm skana
```

## Docker Compose

Create a `docker-compose.yml`:

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
```

Run with:
```bash
docker-compose up -d
```

## Features in Latest Image

✅ **New PCA Initialization** - Principal Component Analysis for ALS  
✅ Correction Spectra support  
✅ SVD, NMF, Sequential, Restart initializations  
✅ Hybrid Hard-Soft Modeling (HH-SM)  
✅ Kinetic modeling with ODEs  
✅ Ambiguity analysis  
✅ Full MCR-ALS capabilities  

## R Packages Included

- shiny, shinythemes, shinycssloaders, shinyBS
- DT, nnls, Iso, NMFN
- deSolve, Rsolnp, rgenoud
- fields, mvtnorm, msm, xtable
- callr, processx
- RColorBrewer, viridisLite
- changepoint, outliers

## Troubleshooting

### Container won't start
```bash
# Check logs
docker logs skana

# Remove and recreate
docker rm skana
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

### Port already in use
```bash
# Use different port
docker run -d -p 8080:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

### Permission denied on Linux
```bash
# Run with sudo or add user to docker group
sudo docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

## Building from Source

If you want to build locally:

```bash
# Clone repository
git clone https://github.com/Saldenisov/SK-Ana.git
cd SK-Ana

# Build image
docker build -t saldenisov/skana:latest -f Dockerfile .

# Run
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

## Updating to Latest Version

```bash
# Stop and remove old container
docker stop skana
docker rm skana

# Pull latest image
docker pull saldenisov/skana:latest

# Run new container
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

## Data Persistence

To persist data and outputs:

```bash
# Create local directory
mkdir -p ~/skana-data

# Mount volume
docker run -d \
  -p 3840:3840 \
  -v ~/skana-data:/SK-Ana/outputDir \
  --name skana \
  -e PORT=3840 \
  saldenisov/skana:latest
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | 3000 | Internal Shiny server port |

## Health Check

```bash
# Check if container is running
docker ps --filter name=skana

# Check application health
curl http://localhost:3840

# View real-time logs
docker logs -f skana
```

## Support

- **GitHub:** https://github.com/Saldenisov/SK-Ana
- **Docker Hub:** https://hub.docker.com/r/saldenisov/skana
- **Issues:** https://github.com/Saldenisov/SK-Ana/issues

## Version History

- **latest (2025-11-19):** Added PCA initialization for ALS
- **Previous:** Correction Spectra feature
- **Base:** Full MCR-ALS and kinetic modeling suite

---

**Last Updated:** 2025-11-19  
**Image Digest:** sha256:7d5c1b7e4ecf7cd5a26595dfb057bf22be8e5572ac4107e7a8a5175f1b3cc973
