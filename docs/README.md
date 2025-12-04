# SK-Ana Documentation

Complete documentation for SK-Ana project.

## Quick Links

- **[Main README](../README.md)** - Start here for overview and basic usage
- **[User Manual](https://saldenisov.github.io/SK-Ana/)** - Online user guide

---

## Deployment Documentation

Docker deployment and CI/CD information.

| Document | Description |
|----------|-------------|
| **[DOCKER_PLATFORM_GUIDE.md](deployment/DOCKER_PLATFORM_GUIDE.md)** | ⭐ **START HERE** - Platform-specific quick reference |
| **[DEPLOYMENT_CHECKLIST.md](deployment/DEPLOYMENT_CHECKLIST.md)** | Step-by-step deployment guide |
| **[TASK_COMPLETE.md](deployment/TASK_COMPLETE.md)** | Deployment completion summary |
| **[CICD.md](deployment/CICD.md)** | CI/CD pipeline architecture |
| **[DOCKER_UPDATES_2025-12-04.md](deployment/DOCKER_UPDATES_2025-12-04.md)** | Latest Docker updates |
| **[DOCKER_HUB_VERIFICATION.md](deployment/DOCKER_HUB_VERIFICATION.md)** | Docker Hub verification report |
| **[DOCKER_DEPLOY.md](deployment/DOCKER_DEPLOY.md)** | Deployment procedures |
| **[DOCKER_SCRIPTS_README.md](deployment/DOCKER_SCRIPTS_README.md)** | Docker scripts documentation |

### Also See (Root Level)
- [DOCKER.md](../DOCKER.md) - Complete Docker deployment guide
- [README_DOCKER.md](../README_DOCKER.md) - Cross-platform Docker instructions
- [.github/SETUP.md](../.github/SETUP.md) - GitHub Actions setup

---

## Development Documentation

Technical implementation details and development guides.

| Document | Description |
|----------|-------------|
| **[CODEBASE_OVERVIEW.md](development/CODEBASE_OVERVIEW.md)** | Codebase structure and organization |
| **[COMMIT_SUMMARY.md](development/COMMIT_SUMMARY.md)** | Recent changes and commit details |
| **[PCA_INITIALIZATION.md](development/PCA_INITIALIZATION.md)** | PCA initialization implementation |
| **[PER_COMPONENT_CONSTRAINTS.md](development/PER_COMPONENT_CONSTRAINTS.md)** | Component constraints documentation |
| **[EXPLANATION.md](development/EXPLANATION.md)** | Technical explanations |

### Also See (Root Level)
- [CHANGELOG_2025-11-19.md](../CHANGELOG_2025-11-19.md) - Recent changelog

---

## User Documentation

User-facing documentation for using SK-Ana.

Located in `docs/` directory:
- [about.md](about.md) - About SK-Ana
- [als.md](als.md) - ALS algorithm
- [data.md](data.md) - Data formats
- [downloads.md](downloads.md) - Download options
- [correction_spectra.md](correction_spectra.md) - Spectra correction
- [broadening_*.md](.) - Broadening optimization docs
- [debug_console.md](debug_console.md) - Debug console usage

---

## Documentation Organization

```
SK-Ana/
├── README.md                          # Main project README
├── DOCKER.md                          # Docker deployment guide
├── README_DOCKER.md                   # Cross-platform Docker instructions
├── CHANGELOG_2025-11-19.md            # Recent changes
├── Dockerfile                         # Docker build (amd64)
├── Dockerfile.arm64                   # Docker build (arm64)
├── .github/
│   ├── SETUP.md                       # GitHub Actions setup
│   └── workflows/
│       └── docker-build-push.yml      # CI/CD workflow
└── docs/
    ├── README.md                      # This file
    ├── deployment/                    # Deployment docs
    │   ├── DOCKER_PLATFORM_GUIDE.md   # ⭐ Platform guide
    │   ├── DEPLOYMENT_CHECKLIST.md    # Deployment steps
    │   ├── TASK_COMPLETE.md           # Completion summary
    │   ├── CICD.md                    # CI/CD details
    │   └── ...
    ├── development/                   # Development docs
    │   ├── CODEBASE_OVERVIEW.md       # Code structure
    │   ├── COMMIT_SUMMARY.md          # Recent commits
    │   └── ...
    └── *.md                           # User documentation
```

---

## Getting Started

### For Users
1. Read the [Main README](../README.md)
2. Follow [DOCKER_PLATFORM_GUIDE.md](deployment/DOCKER_PLATFORM_GUIDE.md) for your platform
3. Check the [User Manual](https://saldenisov.github.io/SK-Ana/)

### For Deployers
1. Follow [DEPLOYMENT_CHECKLIST.md](deployment/DEPLOYMENT_CHECKLIST.md)
2. Review [CICD.md](deployment/CICD.md) for CI/CD setup
3. Check [TASK_COMPLETE.md](deployment/TASK_COMPLETE.md) for status

### For Developers
1. Review [CODEBASE_OVERVIEW.md](development/CODEBASE_OVERVIEW.md)
2. Check [COMMIT_SUMMARY.md](development/COMMIT_SUMMARY.md) for recent changes
3. See technical docs in [development/](development/)

---

**Last Updated**: December 4, 2025
