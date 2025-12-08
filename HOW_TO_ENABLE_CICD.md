# How to Enable Automated Docker Builds (CI/CD)

## What is a Docker Hub Token?

A **Docker Hub Token** is like a password that allows GitHub to push your Docker images to Docker Hub automatically.

Think of it like this:
- **Docker Hub** = Where your Docker images are stored (like Google Drive for Docker)
- **GitHub Actions** = Robot that builds your images automatically
- **Token** = Special password that lets the robot upload to Docker Hub

### Why do we need it?

Without the token, GitHub Actions can build your images but **cannot push them to Docker Hub**. The token gives permission to upload.

---

## Step-by-Step: Enable CI/CD (5 minutes)

### Step 1: Create Docker Hub Token

1. **Go to Docker Hub**
   - Open browser: https://hub.docker.com/
   - Login with username: `saldenisov`

2. **Navigate to Security Settings**
   - Click your **profile icon** (top right corner)
   - Click **Account Settings**
   - Click **Security** (left sidebar)

3. **Create Access Token**
   - Scroll down to **"Access Tokens"** section
   - Click **"New Access Token"** button
   
4. **Fill in the form**:
   - **Description**: `github-actions-skana` (you can name it anything)
   - **Access permissions**: Select **"Read, Write, Delete"**
   - Click **"Generate"** button

5. **Copy the Token** ⚠️ IMPORTANT
   - A long string will appear (looks like: `dckr_pat_abc123xyz...`)
   - **Copy it immediately!** You won't see it again
   - Save it temporarily (in a text file or notes)

---

### Step 2: Add Token to GitHub

1. **Go to your GitHub repository**
   - Open: https://github.com/Saldenisov/SK-Ana

2. **Open Settings**
   - Click **"Settings"** tab (top menu)
   - You must be logged in as the repository owner

3. **Navigate to Secrets**
   - In left sidebar, click **"Secrets and variables"**
   - Click **"Actions"**

4. **Add the Secret**
   - Click **"New repository secret"** (green button, top right)
   
5. **Fill in the form**:
   - **Name**: `DOCKER_HUB_TOKEN` (must be EXACTLY this)
   - **Secret**: Paste the token you copied from Docker Hub
   - Click **"Add secret"**

6. **Verify**
   - You should now see `DOCKER_HUB_TOKEN` in the list
   - The value will be hidden (shows as `***`)

---

### Step 3: That's It! ✅

**The build will start automatically** because you already pushed the code!

---

## What Happens Next?

### Automatically (Within minutes):

1. **GitHub Actions detects** you added the token
2. **Workflow starts** building Docker images
3. **Two images are built**:
   - `latest-amd64` for Windows, Linux, Intel Mac
   - `latest-arm64` for Apple Silicon Mac
4. **Multi-platform image created**: `latest` (combines both)
5. **Images pushed to Docker Hub**: https://hub.docker.com/r/saldenisov/skana

### Check Progress:

- **GitHub Actions**: https://github.com/Saldenisov/SK-Ana/actions
- **Docker Hub**: https://hub.docker.com/r/saldenisov/skana/tags

### Build Time:
- First build: ~8-12 minutes
- Subsequent builds: ~2-4 minutes (uses cache)

---

## What Users Get

### Before (Current):
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```
- Windows/Linux/Intel Mac: ✅ Works
- Apple Silicon Mac: ⚠️ Works with platform warning

### After (When CI/CD Completes):
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```
- Windows/Linux/Intel Mac: ✅ Works (same as before)
- Apple Silicon Mac: ✅ Works **WITHOUT warning** (native arm64!)

**Same command, better experience for Apple Silicon users!**

---

## Troubleshooting

### "I don't see the token after creating it"
**That's normal!** Docker Hub shows the token only once for security. If you lost it:
1. Go back to Docker Hub → Security → Access Tokens
2. Delete the old token
3. Create a new one
4. Update the GitHub secret with the new token

### "Build failed with 'denied: requested access to the resource is denied'"
**Problem**: Token is wrong or not set

**Solution**:
1. Go to GitHub → Settings → Secrets and variables → Actions
2. Check if `DOCKER_HUB_TOKEN` exists
3. If it exists, delete it and create a new Docker Hub token
4. Add the new token

### "I don't see Settings tab in GitHub"
**Problem**: You're not logged in as the repository owner

**Solution**: 
- Login as `Saldenisov` (the repository owner)
- Only the owner can add secrets

### "Build is not starting"
**Solution**: Manually trigger it:
1. Go to https://github.com/Saldenisov/SK-Ana/actions
2. Click "Build and Push Docker Images" (left sidebar)
3. Click "Run workflow" button
4. Select branch: `master`
5. Click green "Run workflow" button

---

## Visual Guide

```
┌─────────────────────────────────────────────────────────┐
│  Step 1: Docker Hub                                     │
│  https://hub.docker.com/                                │
│                                                          │
│  Profile Icon → Account Settings → Security             │
│  → Access Tokens → New Access Token                     │
│                                                          │
│  Description: github-actions-skana                      │
│  Permissions: Read, Write, Delete                       │
│  → Generate → Copy Token ✅                             │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  Step 2: GitHub                                         │
│  https://github.com/Saldenisov/SK-Ana                   │
│                                                          │
│  Settings → Secrets and variables → Actions             │
│  → New repository secret                                │
│                                                          │
│  Name: DOCKER_HUB_TOKEN                                 │
│  Secret: [paste token from Step 1]                      │
│  → Add secret ✅                                        │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  Step 3: Automatic                                      │
│                                                          │
│  GitHub Actions starts building...                      │
│  ├── Build amd64 image (3-5 min)                        │
│  ├── Build arm64 image (4-6 min)                        │
│  ├── Create multi-platform manifest                     │
│  └── Push to Docker Hub ✅                              │
│                                                          │
│  Done! Images available on Docker Hub                   │
└─────────────────────────────────────────────────────────┘
```

---

## Summary

**Docker Hub Token** = Special password for GitHub to upload images

**Steps**:
1. Create token on Docker Hub (2 minutes)
2. Add token to GitHub Secrets (1 minute)
3. Wait for build to complete (8-12 minutes)

**Result**: Automated Docker builds for all platforms!

---

## Quick Links

- **Create Token**: https://hub.docker.com/ → Profile → Security → Access Tokens
- **Add Secret**: https://github.com/Saldenisov/SK-Ana/settings/secrets/actions
- **Watch Build**: https://github.com/Saldenisov/SK-Ana/actions
- **Check Images**: https://hub.docker.com/r/saldenisov/skana/tags

---

**Need help?** See detailed guide: `docs/deployment/GITHUB_ACTIONS_SETUP.md`

**Last Updated**: December 8, 2025
