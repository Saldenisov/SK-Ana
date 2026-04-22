#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
MAMBA_ROOT_PREFIX="${REPO_ROOT}/.R_skana/micromamba"
MICROMAMBA_BIN="${REPO_ROOT}/.R_skana/tools/micromamba/bin/micromamba"

"${SCRIPT_DIR}/setup_r_skana.sh"

cd "${REPO_ROOT}"
export SK_ANA_PROJECT_ROOT="${REPO_ROOT}"
export SK_ANA_RENV_ROOT="${REPO_ROOT}/.R_skana/renv"
export RENV_PATHS_ROOT="${SK_ANA_RENV_ROOT}"
export RENV_PATHS_LIBRARY="${SK_ANA_RENV_ROOT}/library"
export RENV_PATHS_CACHE="${SK_ANA_RENV_ROOT}/cache"
export RENV_PATHS_SANDBOX="${SK_ANA_RENV_ROOT}/sandbox"
export RENV_CONFIG_SANDBOX_ENABLED="FALSE"
export RENV_CONFIG_SHIMS_ENABLED="FALSE"

if [[ "$#" -eq 0 ]]; then
  exec "${MICROMAMBA_BIN}" run -r "${MAMBA_ROOT_PREFIX}" -n R_skana R
fi

exec "${MICROMAMBA_BIN}" run -r "${MAMBA_ROOT_PREFIX}" -n R_skana "$@"
