# SK-Ana Docker Rebuild Scripts

Quick reference for rebuilding and running SK-Ana Docker container after code changes.

## Scripts

### 1. `rebuild-docker.bat` - Full Rebuild
**Use this when:** You want a complete fresh build (removes old image)

**What it does:**
1. Checks Docker is running
2. Stops and removes existing container
3. Removes old Docker image (forces fresh build)
4. Builds new image from scratch
5. Starts container
6. Shows logs

**Usage:**
```cmd
rebuild-docker.bat
```

**Time:** ~3-5 minutes (full build)

---

### 2. `quick-rebuild.bat` - Fast Rebuild
**Use this when:** You made code changes and want quick iteration

**What it does:**
1. Stops and removes existing container
2. Rebuilds image (uses Docker cache - much faster!)
3. Starts container

**Usage:**
```cmd
quick-rebuild.bat
```

**Time:** ~30 seconds - 2 minutes (cached build)

---

### 3. `view-logs.bat` - View Logs
**Use this when:** Container is running and you want to see live logs

**What it does:**
- Shows live streaming logs from the running container
- Press `Ctrl+C` to exit

**Usage:**
```cmd
view-logs.bat
```

---

## Configuration

All scripts use these settings (edit at the top of each file if needed):

```batch
IMAGE_NAME=skana
IMAGE_TAG=latest
CONTAINER_NAME=skana
PORT=3840
DOCKERFILE=Dockerfile
```

---

## Access the App

After running either rebuild script, access SK-Ana at:
```
http://localhost:3840
```

---

## Manual Docker Commands

If you prefer manual control:

### Stop container
```cmd
docker stop skana
```

### Remove container
```cmd
docker rm skana
```

### Remove image
```cmd
docker rmi skana:latest
```

### Build image
```cmd
docker build -t skana:latest .
```

### Run container
```cmd
docker run -d -p 3840:3840 --name skana -e PORT=3840 skana:latest
```

### View logs
```cmd
docker logs -f skana
```

### Execute commands in container
```cmd
docker exec -it skana bash
```

---

## Troubleshooting

### Docker daemon not running
**Error:** `Cannot connect to the Docker daemon`

**Fix:** Start Docker Desktop

### Port already in use
**Error:** `Bind for 0.0.0.0:3840 failed: port is already allocated`

**Fix:** Stop the existing container first:
```cmd
docker stop skana
docker rm skana
```

### Build fails
**Check:**
1. Is Docker Desktop running?
2. Do you have enough disk space?
3. Check the error message - it will tell you which step failed

### Container starts but app doesn't load
**Fix:** Check logs:
```cmd
docker logs skana
```

Look for R errors or missing packages.

---

## Development Workflow

**Typical workflow after making code changes:**

1. Make changes to R files (ui.R, server.R, etc.)
2. Run `quick-rebuild.bat`
3. Wait ~1-2 minutes
4. Refresh browser at `http://localhost:3840`
5. Test changes
6. Repeat!

**For major changes (new packages, Dockerfile changes):**
- Use `rebuild-docker.bat` for a clean build

---

## Notes

- `quick-rebuild.bat` is recommended for most development work
- `rebuild-docker.bat` ensures a clean slate but takes longer
- The container runs in detached mode (-d flag) so it runs in background
- Logs are available even after the script exits via `view-logs.bat`
