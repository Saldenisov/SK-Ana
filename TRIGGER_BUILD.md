# How to Trigger GitHub Actions Build

## ✅ Build Just Started!

I triggered it for you by pushing a commit. Check here:
**https://github.com/Saldenisov/SK-Ana/actions**

You should see "Build and Push Docker Images" running now.

---

## Why Didn't It Start Automatically?

The workflow only triggers when you **push code changes** to the `master` branch.

**What happened**:
1. Dec 4: Workflow file was added (build tried but failed - no token yet)
2. Dec 5-8: You added docs and status files, but GitHub Pages ran instead
3. **Now**: I pushed an empty commit to trigger the Docker build

---

## How to Manually Trigger a Build (3 Ways)

### Method 1: Push Any Change (Easiest)

```bash
# Create an empty commit
git commit --allow-empty -m "Trigger build"
git push origin master
```

This triggers the workflow immediately.

---

### Method 2: Via GitHub Web Interface

1. **Go to Actions page**
   - Open: https://github.com/Saldenisov/SK-Ana/actions

2. **Find the workflow**
   - In the left sidebar, click **"Build and Push Docker Images"**

3. **Trigger manually**
   - Look for **"Run workflow"** button (gray button, right side)
   - Click it
   - A dropdown appears:
     - Branch: select **master**
     - Click green **"Run workflow"** button

4. **Watch it run**
   - A new workflow run will appear in the list
   - Click on it to see progress

---

### Method 3: Make Any Code Change

The workflow triggers automatically when you push **any** change to these files:
- R files (`.R`)
- Dockerfiles
- Application files (ui_files/, server_files/, data/)

**But NOT for**:
- Markdown files (`.md`)
- Documentation in `docs/`
- `.gitignore`

This is configured in the workflow file to avoid unnecessary builds for doc-only changes.

---

## Check Build Status

### 1. GitHub Actions Page
**https://github.com/Saldenisov/SK-Ana/actions**

You'll see:
- Workflow name: "Build and Push Docker Images"
- Status: 
  - 🟡 Yellow circle = Running
  - ✅ Green check = Success
  - ❌ Red X = Failed

### 2. Click on a Run
- See detailed steps
- View logs for each step
- Check build time
- See if images were pushed

### 3. Build Summary
At the end of a successful build, you'll see:
- Images published to Docker Hub
- Tags: `latest`, `latest-amd64`, `latest-arm64`
- Usage instructions

---

## Timeline

**What's happening right now** (after I triggered it):

1. **0-1 min**: GitHub Actions starts
2. **1-5 min**: Build amd64 image
3. **1-5 min**: Build arm64 image (parallel)
4. **5-8 min**: Create multi-platform manifest
5. **8-10 min**: Push all images to Docker Hub
6. **10 min**: ✅ Done!

---

## After the Build Succeeds

### 1. Check Docker Hub
**https://hub.docker.com/r/saldenisov/skana/tags**

You should see:
- `latest` (multi-platform: amd64, arm64)
- `latest-amd64` 
- `latest-arm64`

### 2. Verify Multi-Platform

On your Mac:
```bash
docker manifest inspect saldenisov/skana:latest
```

Should show:
```json
"manifests": [
  {
    "platform": {
      "architecture": "amd64",
      "os": "linux"
    }
  },
  {
    "platform": {
      "architecture": "arm64",
      "os": "linux"
    }
  }
]
```

### 3. Test the Image

```bash
# Pull new image
docker pull saldenisov/skana:latest

# Check architecture (should show arm64 on your Mac)
docker image inspect saldenisov/skana:latest --format='{{.Architecture}}'

# Run it
docker stop skana && docker rm skana
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest

# Check - NO platform warning! ✅
docker ps
```

---

## Future Builds

From now on, the build will trigger automatically when you:

1. **Push code changes**:
   ```bash
   git add .
   git commit -m "Update something"
   git push origin master
   ```
   → Build starts automatically

2. **Merge a pull request** to master
   → Build starts automatically

3. **Create a release** on GitHub
   → Build starts automatically

---

## Troubleshooting

### Build Failed?

1. **Check the logs**
   - Go to: https://github.com/Saldenisov/SK-Ana/actions
   - Click on the failed run
   - Click on the red X to see error

2. **Common issues**:
   - Token wrong: Check `DOCKER_HUB_TOKEN` in GitHub Secrets
   - Network error: Just re-run the workflow
   - File missing: Check Dockerfile and Dockerfile.arm64 exist

### Re-run a Failed Build

1. Go to the failed run
2. Click **"Re-run all jobs"** button (top right)
3. Confirm

---

## Quick Reference

| Action | How |
|--------|-----|
| **Trigger build** | Push to master: `git commit --allow-empty -m "Trigger" && git push` |
| **Check status** | https://github.com/Saldenisov/SK-Ana/actions |
| **View images** | https://hub.docker.com/r/saldenisov/skana/tags |
| **Manual trigger** | Actions → Build and Push Docker Images → Run workflow |

---

## Summary

**Right now**: Build is running (I triggered it for you)

**Check progress**: https://github.com/Saldenisov/SK-Ana/actions

**Wait time**: ~8-12 minutes

**Next time**: Just push any code change, it will build automatically!

---

**Last Updated**: December 8, 2025  
**Status**: Build triggered ✅  
**Expected completion**: ~10 minutes from now
