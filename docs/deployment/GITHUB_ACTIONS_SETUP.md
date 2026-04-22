# GitHub Actions Setup Guide

Simple step-by-step guide to enable automated Docker builds.

---

## Overview

GitHub Actions will automatically build Docker images for Windows, Linux, Intel Mac, and Apple Silicon Mac every time you push code.

**Time required**: 5-10 minutes

---

## Step 1: Create Docker Hub Access Token

### 1.1 Go to Docker Hub
Open your web browser and go to: **https://hub.docker.com/**

### 1.2 Login
Login with your Docker Hub account (`saldenisov`)

### 1.3 Navigate to Security Settings
1. Click on your **profile icon** (top right)
2. Select **Account Settings**
3. Click on **Security** tab in the left sidebar

### 1.4 Create New Access Token
1. Scroll down to **Access Tokens** section
2. Click **New Access Token** button
3. Fill in:
   - **Description**: `github-actions-skana` (or any name you like)
   - **Access permissions**: Select **Read, Write, Delete**
4. Click **Generate** button

### 1.5 Copy the Token
⚠️ **IMPORTANT**: 
- The token will be shown **only once**!
- Copy it immediately and save it somewhere temporarily
- You will need it in the next step

Example token format: `dckr_pat_abc123xyz...`

---

## Step 2: Add Token to GitHub Secrets

### 2.1 Go to Your GitHub Repository
Open: **https://github.com/Saldenisov/SK-Ana**

### 2.2 Navigate to Settings
1. Click on **Settings** tab (top menu bar)
2. In the left sidebar, click **Secrets and variables**
3. Click **Actions**

### 2.3 Add New Secret
1. Click **New repository secret** button (green button, top right)
2. Fill in:
   - **Name**: `DOCKER_HUB_TOKEN` (must be exactly this)
   - **Secret**: Paste the token you copied from Docker Hub
3. Click **Add secret** button

### 2.4 Verify
You should now see `DOCKER_HUB_TOKEN` in the list of secrets.

---

## Step 3: Trigger the Build

The build should start automatically since you already pushed the code.

### 3.1 Check GitHub Actions
1. Go to **https://github.com/Saldenisov/SK-Ana/actions**
2. You should see a workflow run named "Build and Push Docker Images"
3. Click on it to see the progress

### 3.2 If No Build Appears

**Option A: Manual Trigger**
1. Go to https://github.com/Saldenisov/SK-Ana/actions
2. In the left sidebar, click **Build and Push Docker Images**
3. Click **Run workflow** button (right side, gray button)
4. Select branch: **master**
5. Click green **Run workflow** button

**Option B: Push a Small Change**
```bash
# Make a small change and push
git commit --allow-empty -m "Trigger CI/CD build"
git push origin master
```

---

## Step 4: Monitor the Build

### 4.1 Watch Build Progress
1. In GitHub Actions, click on the running workflow
2. You'll see steps like:
   - Checkout code
   - Set up QEMU
   - Build amd64 image
   - Build arm64 image
   - Create multi-platform manifest
   - Push to Docker Hub

### 4.2 Build Time
- **First build**: ~8-12 minutes (downloads base images)
- **Subsequent builds**: ~2-4 minutes (uses cache)

### 4.3 Success Indicators
✅ Green checkmark appears next to workflow  
✅ All steps show green checkmarks  
✅ Build summary shows images pushed  

---

## Step 5: Verify Docker Hub

### 5.1 Check Docker Hub
1. Go to **https://hub.docker.com/r/saldenisov/skana/tags**
2. You should see three new tags:
   - `latest` (multi-platform)
   - `latest-amd64`
   - `latest-arm64`

### 5.2 Verify Multi-Platform
On your local machine:
```bash
docker manifest inspect saldenisov/skana:latest
```

Should show two platforms:
- `linux/amd64`
- `linux/arm64`

---

## Step 6: Test the Images

### 6.1 Pull and Run
```bash
# Remove old local images
docker rmi saldenisov/skana:latest 2>/dev/null || true

# Pull new multi-platform image
docker pull saldenisov/skana:latest

# Check architecture
docker image inspect saldenisov/skana:latest --format='{{.Architecture}}'
# On Apple Silicon: should show "arm64"
# On Intel/AMD: should show "amd64"

# Run container
docker stop skana && docker rm skana 2>/dev/null || true
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest

# Access app
open http://localhost:3840
```

### 6.2 Verify No Warnings
On Apple Silicon Mac, you should see **NO platform warnings** now!

---

## That's It! 🎉

Your CI/CD is now active. From now on:

- **Every push to master** → Automatic build
- **Build succeeds** → Images published to Docker Hub
- **Users pull** → Get correct architecture automatically

---

## Troubleshooting

### Build Fails: "denied: requested access to the resource is denied"

**Problem**: `DOCKER_HUB_TOKEN` is missing or incorrect

**Solution**:
1. Go to GitHub → Settings → Secrets and variables → Actions
2. Verify `DOCKER_HUB_TOKEN` exists
3. If not, create it (Step 2 above)
4. If exists but fails, create a new Docker Hub token and update the secret

### Build Fails: "failed to solve"

**Problem**: Network issue or missing files

**Solution**:
1. Check build logs for specific error
2. Retry the build (usually transient)
3. Ensure all files (Dockerfile, Dockerfile.arm64) are in repo

### Images Not on Docker Hub

**Problem**: Build ran but images not pushed

**Check**:
1. Was it a pull request? (PRs don't push, only build)
2. Check build logs for push confirmation
3. Verify Docker Hub token has write permissions

### Workflow Doesn't Trigger

**Solution**:
1. Manually trigger (Step 3.2, Option A)
2. Check workflow file is in `.github/workflows/`
3. Ensure you're pushing to `master` branch

---

## Quick Reference

| What | Where |
|------|-------|
| **Docker Hub** | https://hub.docker.com/ |
| **GitHub Actions** | https://github.com/Saldenisov/SK-Ana/actions |
| **Repository Settings** | https://github.com/Saldenisov/SK-Ana/settings |
| **Add Secrets** | https://github.com/Saldenisov/SK-Ana/settings/secrets/actions |

---

## Next Steps

✅ GitHub Actions configured  
✅ Multi-platform builds enabled  
✅ Images automatically published  

**Optional enhancements** (future):
- Add version tags (v1.0.0, etc.)
- Add security scanning
- Deploy documentation automatically
- Add status notifications

---

**Last Updated**: December 4, 2025  
**Status**: Ready to use ✅
