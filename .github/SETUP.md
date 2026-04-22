# GitHub Actions Setup Guide

This document explains how to set up automated Docker image builds for SK-Ana.

## Overview

The GitHub Actions workflow automatically builds and pushes Docker images for multiple platforms when code is pushed to the repository.

### What Gets Built

- **amd64 image**: For Windows, Linux, and Intel Mac
- **arm64 image**: For Mac Apple Silicon (M1/M2/M3)
- **Multi-platform manifest**: Automatically selects the correct architecture

### When Builds Trigger

Automatic builds are triggered on:
- Push to `main` or `master` branch (excluding markdown/doc changes)
- Pull requests (build only, no push)
- Release creation
- Manual trigger via GitHub Actions UI

## Prerequisites

### 1. Docker Hub Account

You need a Docker Hub account to push images.

**Current configuration:**
- Docker Hub username: `saldenisov`
- Repository: `saldenisov/skana`

### 2. Docker Hub Access Token

Create a Docker Hub access token with read/write permissions:

1. Go to [Docker Hub](https://hub.docker.com/)
2. Click on your profile → **Account Settings**
3. Go to **Security** → **Access Tokens**
4. Click **New Access Token**
5. Name: `github-actions-skana` (or any name you prefer)
6. Permissions: **Read, Write, Delete**
7. Click **Generate**
8. **Copy the token immediately** (you won't see it again)

### 3. Add Token to GitHub Secrets

Add the Docker Hub token to your GitHub repository:

1. Go to your GitHub repository: https://github.com/Saldenisov/SK-Ana
2. Click **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret**
4. Name: `DOCKER_HUB_TOKEN`
5. Value: Paste the Docker Hub access token
6. Click **Add secret**

## Workflow File

The workflow is defined in `.github/workflows/docker-build-push.yml`

### Key Features

- **Multi-platform builds**: Builds both amd64 and arm64 images
- **Build caching**: Uses Docker registry cache for faster builds
- **Automatic tagging**: Tags images as `latest`, `latest-amd64`, `latest-arm64`
- **Pull request builds**: Tests builds on PRs without pushing
- **Manual triggers**: Can be triggered manually from GitHub UI
- **Build summaries**: Provides detailed build information in GitHub Actions

## Usage

### Automatic Builds

Simply push your code:

```bash
git add .
git commit -m "Update SK-Ana"
git push origin main
```

GitHub Actions will automatically:
1. Build the amd64 image using `Dockerfile`
2. Build the arm64 image using `Dockerfile.arm64`
3. Create a multi-platform manifest combining both
4. Push all images to Docker Hub

### Manual Trigger

To manually trigger a build:

1. Go to https://github.com/Saldenisov/SK-Ana/actions
2. Click on "Build and Push Docker Images" workflow
3. Click **Run workflow** button
4. Select branch (usually `main`)
5. Click **Run workflow**

### Viewing Build Status

Check build status at:
- https://github.com/Saldenisov/SK-Ana/actions

Each build shows:
- Build logs for each platform
- Image tags created
- Multi-platform manifest details
- Usage instructions

## Published Images

After a successful build, images are available at:

**Docker Hub repository:** https://hub.docker.com/r/saldenisov/skana

**Available tags:**
- `latest` - Multi-platform (auto-selects amd64 or arm64)
- `latest-amd64` - Specifically for amd64 (Windows, Linux, Intel Mac)
- `latest-arm64` - Specifically for arm64 (Apple Silicon Mac)

## User Experience

### For End Users

**Windows/Linux/Intel Mac users:**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```
→ Automatically gets the amd64 image

**Mac Apple Silicon users:**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```
→ Automatically gets the arm64 image (no more platform warnings!)

**Explicit architecture selection:**
```bash
# Force amd64
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest-amd64

# Force arm64
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest-arm64
```

## Build Time

Approximate build times:
- **amd64 build**: 3-5 minutes (first build), 1-2 minutes (cached)
- **arm64 build**: 4-6 minutes (first build), 1-2 minutes (cached)
- **Total workflow**: 8-12 minutes (first build), 2-4 minutes (cached)

## Troubleshooting

### Build Fails with "denied: requested access to the resource is denied"

**Solution:** Check that `DOCKER_HUB_TOKEN` secret is set correctly:
1. Go to repository Settings → Secrets and variables → Actions
2. Verify `DOCKER_HUB_TOKEN` exists
3. If not, create it with your Docker Hub access token

### Build Fails on arm64

**Common causes:**
- Missing dependencies in `Dockerfile.arm64`
- Network issues pulling base images
- Insufficient build resources

**Solution:** Check the build logs for specific errors and ensure `Dockerfile.arm64` is up to date.

### Images Not Pushed (build succeeds but images missing)

**Cause:** This happens for pull request builds (by design)

**Solution:** Merge the PR to main/master branch to trigger a push build.

### Manual Trigger Doesn't Work

**Solution:** Ensure you have write access to the repository.

## Maintenance

### Updating Docker Hub Username

If you need to change the Docker Hub username:

1. Edit `.github/workflows/docker-build-push.yml`
2. Change the `DOCKER_USERNAME` environment variable:
   ```yaml
   env:
     DOCKER_USERNAME: your-new-username
     IMAGE_NAME: skana
   ```
3. Update the `DOCKER_HUB_TOKEN` secret with the new account's token

### Adding Version Tags

To add version-based tagging (e.g., v1.0.0):

1. Create a git tag:
   ```bash
   git tag -a v1.0.0 -m "Release version 1.0.0"
   git push origin v1.0.0
   ```

2. Create a GitHub release from the tag

3. The workflow will automatically create images tagged as:
   - `1.0.0`
   - `1.0`
   - `1`
   - `latest`

## Monitoring

### Check Build Status Badge

Add to README.md:

```markdown
[![Docker Build](https://github.com/Saldenisov/SK-Ana/actions/workflows/docker-build-push.yml/badge.svg)](https://github.com/Saldenisov/SK-Ana/actions/workflows/docker-build-push.yml)
```

### Email Notifications

GitHub sends email notifications on:
- Build failures
- First successful build after failures

Configure in: GitHub Settings → Notifications

## Security

### Best Practices

1. **Never commit Docker Hub tokens** to the repository
2. **Use GitHub Secrets** for all sensitive data
3. **Rotate access tokens** periodically (every 6-12 months)
4. **Use scoped tokens** (read/write only, not admin)
5. **Review workflow logs** for any security issues

### Token Permissions

The `DOCKER_HUB_TOKEN` needs:
- ✅ Read
- ✅ Write
- ❌ Admin (not required)

## Cost

GitHub Actions provides:
- **Public repositories**: Unlimited minutes (free)
- **Private repositories**: 2,000 minutes/month (free tier)

Docker Hub provides:
- **Free tier**: Unlimited public repositories
- **Rate limits**: 200 pulls per 6 hours for anonymous users

## Next Steps

1. ✅ Create Docker Hub access token
2. ✅ Add `DOCKER_HUB_TOKEN` to GitHub Secrets
3. ✅ Push code to trigger first build
4. ✅ Verify images on Docker Hub
5. ✅ Update documentation with new image usage

## Support

- **GitHub Actions docs**: https://docs.github.com/en/actions
- **Docker Hub**: https://hub.docker.com/
- **Workflow file**: `.github/workflows/docker-build-push.yml`

---

**Last Updated:** 2025-12-04
**Status:** Ready to use ✅
